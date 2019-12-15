#!/bin/env ruby

$line = IO.read(ARGV[0]).strip

Game = Struct.new(:num_players, :last_marble_score)

Marble = Struct.new(:score, :ccw, :cw) do
  def remove_from_game
    ccw.cw = cw if ccw
    cw.ccw = ccw if cw
    cw = ccw = nil
  end

  def insert_cw_of(link)
    self.cw = link.cw
    cw.ccw = self if cw
    self.ccw = link
    link.cw = self
  end
end

class Part1And2
  def call_1
    game = parse($line)
    play(game)
  end

  def call_2
    game = parse($line)
    game.last_marble_score *= 100
    play(game)
  end

  private

  def parse(str)
    m = str.match(/(\d+) players.*worth (\d+)/)
    Game.new(*m.captures.map(&:to_i))
  end

  def play(game)
    current_marbel = Marble.new(0)
    current_marbel.ccw = current_marbel
    current_marbel.cw = current_marbel

    player_scores = Array.new(game.num_players, 0)
    active_player = 0

    next_marble_score = 0;
    while current_marbel.score < game.last_marble_score
      marble = Marble.new(next_marble_score += 1)

      if marble.score % 23 == 0
        ccw_7 = 7.times.inject(current_marbel) { |link| link.ccw }
        current_marbel = ccw_7.cw
        ccw_7.remove_from_game

        player_scores[active_player] += marble.score + ccw_7.score
      else
        marble.insert_cw_of(current_marbel.cw)
        current_marbel = marble
      end

      active_player = (active_player + 1) % game.num_players
    end

    player_scores.max
  end
end

printf("1: %p\n2: %p\n",
            Part1And2.new.call_1,
            Part1And2.new.call_2)
