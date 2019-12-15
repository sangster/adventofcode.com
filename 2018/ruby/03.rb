#!/bin/env ruby
require 'set'

$lines = IO.read(ARGV[0]).lines.map(&:strip)

Claim = Struct.new(:name, :x, :y, :w, :h)

class Part1And2
  def new_sparse_grid
    Hash.new do |grid, x|
      grid[x] =
        Hash.new do |row, y|
          row[y] = [0, []]
        end
    end
  end

  def call_count
    grid = new_sparse_grid
    claims.each { |claim| fill(grid, claim) }
    count_conflicts(grid)
  end

  def call_good_one
    grid = new_sparse_grid
    unconflicted = Set.new(claims.to_a)
    unconflicted.each { |claim| fill(grid, claim) }

    grid.values.each do |row|
      row.values.select { |v| v.first > 1 }.each do |val|
        val.last.each { |v| unconflicted.delete(v) }
      end
    end

    unconflicted.first.name
  end

  def count_conflicts(grid)
    grid.values.inject(0) do |count, row|
      count + row.values.inject(0) { |c, val| c + (val.first > 1 ? 1 : 0) }
    end
  end

  def claims
    Enumerator.new do |y|
      $lines.each do |str|
        y.yield(parse(str))
      end
    end
  end

  def parse(str)
    m = /#(\d+)\s+@\s+(\d+),(\d+):\s(\d+)x(\d+)/.match(str)
    Claim.new(m[1].to_i, m[2].to_i, m[3].to_i, m[4].to_i, m[5].to_i)
  end

  def fill(grid, claim)
    (claim.x ... claim.x + claim.w).each do |x|
      (claim.y ... claim.y + claim.h).each do |y|
        grid[x][y][0] += 1
        grid[x][y][1] << claim
      end
    end
  end
end

printf("1: %p\n2: %p\n",
            Part1And2.new.call_count,
            Part1And2.new.call_good_one)
