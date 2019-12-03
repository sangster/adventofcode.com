#!/bin/env ruby
require 'pry-byebug'
require 'colorize'

class Puzzle
  def initialize(nums)
    @improv = 10
    @initial = [3, 7]
    @nums = nums.strip.split('').map(&:to_i)
  end

  def call_1
    count = @nums.join.to_i
    score = @initial.dup
    a = 0
    b = 1

    loop do
      score_a, score_b = (score[a] + score[b]).divmod(10)
      score << score_a if score_a != 0
      score << score_b

      a = (a + score[a] + 1) % score.size
      b = (b + score[b] + 1) % score.size
      b = (b + 1) % score.size if a == b

      if score.size >= count + @improv
        return score[-(@improv)..-1].join.to_i
      end
    end
  end

  def call_2
    target = @nums
    score = @initial.dup
    a = 0
    b = 1

    loop do
      # score_a, score_b = (score[a] + score[b]).divmod(10)
      sum = score[a] + score[b]
      score_a = sum / 10
      score_b = sum % 10

      score << score_a if score_a != 0
      score << score_b

      a = (a + score[a] + 1) % score.size
      b = (b + score[b] + 1) % score.size
      b = (b + 1) % score.size if a == b

      # puts render(score, a, b)
      puts render(score[-6..-1] || [], nil, nil)
      # binding.pry
      if score.last == target.last && target == score[(-target.size)..-1]
        return score.size - target.size
      end
    end
  end

  def go(score)
  end


  def render(score, a, b)
    score.each_with_index.map do |s, i|
      case i
      when a then s.to_s.red
      when b then s.to_s.blue.bold
      else        s
      end
    end.join(' ')
  end
end

puzzle = Puzzle.new(ARGV[0])
printf("1: %p\n2: %p\n", puzzle.call_1, puzzle.call_2)
