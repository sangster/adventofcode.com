#!/bin/env ruby

require 'forwardable'
require 'set'


class World
  attr_reader :cells, :elves, :goblins, :units, :width

  def initialize(cells, elves, goblins)
    @cells = cells
    @elves = elves
    @goblins = goblins
    @units = elves + goblins

    @cells.each { |row| row.each { |c| c&.world = self } }
    @width = @cells.first.size
  end

  def cell(x, y)
    @cells[y]&.send(:[], x)
  end

  def turn_order
    (elves + goblins).sort_by(&:cell)
  end
end


class Cell
  attr_reader :x, :y, :unit
  attr_accessor :world

  def initialize(x, y)
    @x = x
    @y = y
    @unit = nil
  end

  def unit=(unit)
    if unit
      raise format('double occupancy on %p', self) if @unit

      unit.cell&.unit = nil
      unit.cell = self
    end
    @unit = unit
  end

  def adj_cells
    @adj_cells ||=
      [world.cell(x, y - 1), world.cell(x - 1, y),
       world.cell(x + 1, y), world.cell(x, y + 1)].compact
  end

  def adj_units
    adj_cells.map(&:unit).compact
  end

  def adj_open
    adj_cells.reject(&:unit)
  end

  def adj?(other)
    x_diff = (x - other.x).abs
    y_diff = (y - other.y).abs

    x_diff == 1 && y_diff == 0 || x_diff == 0 && y_diff == 1
  end

  def dist(other)
    queue = [self]
    visited = Set.new
    backtrack = { self => nil }

    while queue.any?
      cursor = queue.shift
      if cursor == other
        depth = -1
        while cursor
          cursor = backtrack[cursor]
          depth += 1
        end
        return depth
      end

      cursor.adj_open.each do |cell|
        next if visited.include?(cell) || queue.include?(cell)

        backtrack[cell] = cursor
        queue << cell
      end

      visited.add(cursor)
    end

    return nil # no path to other
  end

  def <=>(other)
    [y, x] <=> [other.y, other.x]
  end
end


class Unit
  extend Forwardable

  attr_accessor :cell
  attr_reader :health, :power

  def_delegators :cell, :x, :y, :adj_open, :adj?, :world

  def initialize(power: 3)
    @cell = nil
    @health = 200
    @power = power
  end

  def act
    unless attack_adjacent
      move_toward_closest_enemy
      attack_adjacent
    end
  end

  def move_toward_closest_enemy
    target_adjs = targets.flat_map(&:adj_open).sort.uniq

    dists = Hash.new { |hash, key| hash[key] = [] }
    adj_open.map do |adj_cell|
      target_adjs.each do |target|
        dist = adj_cell.dist(target)
        dists[dist] << [target, adj_cell] if dist
      end
    end
    closest = dists[dists.keys.min].sort_by(&:first).first.last if dists.any?

    closest&.unit = self
  end

  def attack_adjacent
    targets.select { |t| t.adj?(self) }.sort.first
      .tap { |defender| defender&.take_damage(power) }
  end

  def take_damage(amount)
    @health -= amount
    die if health <= 0
  end

  def die
    cell.unit = nil
  end

  def dead?
    health <= 0
  end

  def <=>(other)
    [health, cell] <=> [other.health, other.cell]
  end
end


class Elf < Unit
  def_delegator :world, :goblins, :targets

  def die
    world.elves.delete(self)
    super
  end
end


class Goblin < Unit
  def_delegator :world, :elves, :targets

  def die
    world.goblins.delete(self)
    super
  end
end


class Puzzle
  def initialize(path, elf_power: 3)
    @file = IO.read(path)
  end

  def call_1
    @world = parse_input(@file, 3)

    turn = 0

    while @world.elves.any? && @world.goblins.any?
      @world.turn_order.each { |unit| unit.act unless unit.dead? }
      turn += 1
    end

    (turn - 1) * (@world.elves.sum(&:health) + @world.goblins.sum(&:health))
  end

  def call_2
    elf_power = 4

    loop do
      begin
        @world = parse_input(@file, elf_power)

        turn = 0

        while @world.elves.any? && @world.goblins.any?
          order = @world.turn_order

          order.each { |unit| unit.act unless unit.dead? }
          turn += 1

          raise if @world.units.select { |u| u.is_a?(Elf) && u.dead? }.any?
        end

        hp = (@world.elves.sum(&:health) + @world.goblins.sum(&:health))
        return (turn - 1) * hp
      rescue
        elf_power += 1
      end
    end
  end

  private

  def parse_input(body, elf_power)
    elves = []
    goblins = []

    cells =
      body.lines.each_with_index.map do |line, y|
        line.strip.chars.each_with_index.map do |ch, x|
          next if ch == '#'

          Cell.new(x, y).tap do |cell|
            elves << (cell.unit = Elf.new(power: elf_power)) if ch == 'E'
            goblins << (cell.unit = Goblin.new) if ch == 'G'
          end
        end
      end

    World.new(cells, elves, goblins)
  end
end


puzzle = Puzzle.new(ARGV[0])
printf("1: %p\n2: %p\n", puzzle.call_1, puzzle.call_2)
