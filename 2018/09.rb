#!/bin/env ruby

$line = IO.read(ARGV[0]).strip

Game = Struct.new(:players, :last)

Marble = Struct.new(:val, :ccw, :cw) do
  def remove
    ccw.cw = cw if ccw
    cw.ccw = ccw if cw
    cw = ccw = nil
  end

  def insert_cw_of(link)
    remove
    self.cw = link.cw
    cw.ccw = self if cw
    self.ccw = link
    link.cw = self
  end

  def ccw_n(count = 1)
    count.times.inject(self) { |link| link.ccw }
  end
end

class Part1And2
  def call_1
    game = parse($line)
    play(game)
  end

  def call_2
    game = parse($line)
    game.last *= 100
    play(game)
  end

  private

  def parse(str)
    m = str.match(/(?<p>\d+) players.*worth (?<w>\d+)/)
    Game.new m[:p].to_i, m[:w].to_i
  end

  def play(game)
    curr = Marble.new(0)
    curr.ccw = curr
    curr.cw = curr

    next_marble_value = 0;
    players = Array.new(game.players, 0)
    cur_player = 0

    while curr.val < game.last
      marble = Marble.new(next_marble_value += 1)
      if marble.val % 23 == 0
        players[cur_player] += marble.val
        ccw_7 = curr.ccw_n(7)
        next_curr = ccw_7.cw
        players[cur_player] += ccw_7.val
        ccw_7.remove
        curr = next_curr
      else
        marble.insert_cw_of(curr.cw)
        curr = marble
      end

      cur_player = (cur_player + 1) % game.players
    end

    players.max
  end
end

puts format("1: %p\n2: %p",
            Part1And2.new.call_1,
            Part1And2.new.call_2)
