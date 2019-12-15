#!/bin/env ruby

require 'pry-byebug'
require 'colorize'
require 'forwardable'
require 'set'

$ids = { 'Elf' => 0, 'Goblin' => 0 }

class World
  WALL = '#'.light_black.freeze
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

  def to_s
    @cells.map do |row|
      row.map { |cell| cell ? cell.unit&.ch || ' ' : WALL }.join
    end.join("\n")
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

  def inspect
    format('#<Cell %d,%d%s>', x, y, (format(' %s', unit.ch) if unit))
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
  attr_reader :health, :id, :power

  def_delegators :cell, :x, :y, :adj_open, :adj?, :world

  def initialize(power: 3)
    @cell = nil
    @health = 200
    @id = ($ids[self.class.name] += 1)
    @power = power
  end

  def inspect
    format('#<%s %3d %2d,%2d>', name, health, x, y)
  end

  def name
    @name ||= format('%s%s', ch, format('%02d', id).blue)
  end

  def act
    actions = []
    defender, action = attack_adj
    actions << action if action

    unless defender
      actions << move_toward_closest_enemy
      defender, action = attack_adj
      actions << action if action
    end
    actions.any? ? actions : inspect
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

    action = format("%p %s %p", self, 'MOV'.yellow, closest) if closest
    closest&.unit = self
    action
  end

  def attack_adj
    defender = targets.select { |t| t.adj?(self) }.sort.first
    action = nil
    if defender
      action = format('%p %s %p', self, 'ATT'.red, defender)
      defender.take_damage(power)
    end
    [defender, action]
  end

  def take_damage(amount)
    @health -= amount
    die if health <= 0
  end

  def die
    cell.unit = nil
    @health = 0
  end

  def dead?
    health == 0
  end

  def <=>(other)
    [health, cell] <=> [other.health, other.cell]
  end
end

class Elf < Unit
  ICON = 'E'.green.freeze
  def_delegator :world, :goblins, :targets

  def ch
    ICON
  end

  def die
    world.elves.delete(self)
    super
  end
end

class Goblin < Unit
  ICON = 'G'.red.freeze
  def_delegator :world, :elves, :targets

  def ch
    ICON
  end

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
     @world = parse(@file, 3)

     turn = 0
     clear_screen
     printf("Start\n%s\n\n", render(@world.turn_order.map(&:inspect)))

     while @world.elves.any? && @world.goblins.any?
       [
         Thread.new do
           order = @world.turn_order

           actions = []
           begin
             order.each do |unit|
               actions += Array(unit.act) unless unit.dead?
             end
             turn += 1
             clear_screen
             printf("After %d rounds:\n", turn)
             puts render(actions) # TODO
           ensure
           end
         end,
         Thread.new { sleep 0.25 }
       ].each(&:join)
     end

     hp = (@world.elves.sum(&:health) + @world.goblins.sum(&:health))
     turn -= 1
     clear_screen
     printf("%s win!\nScore:\n", @world.elves.any? ? 'Elves'.green : 'Goblins'.red)
     printf("%d * (%s)\n%d * %d = %d\n",
            turn,
            (@world.elves.map(&:health) + @world.goblins.map(&:health)).join(' + '),
            turn, hp, turn * hp)

     turn * hp
  end

  def call_2
    elf_power = 4
    loop do
      begin
        @world = parse(@file, elf_power)

        turn = 0
        clear_screen
        printf("Start\n%s\n\n", render(@world.turn_order.map(&:inspect)))

        while @world.elves.any? && @world.goblins.any?
          # [
          #   Thread.new do
              order = @world.turn_order

              actions = []
              order.each do |unit|
                actions += Array(unit.act) unless unit.dead?
              end
              turn += 1
              clear_screen
              printf("After %d rounds (%d):\n", turn, elf_power)
              puts render(actions) # TODO

              raise :elf_died if @world.units.select { |u| u.is_a?(Elf) && u.dead? }.any?
            # end,
            # Thread.new { sleep 0.25 }
          # ].each(&:join)
        end

        hp = (@world.elves.sum(&:health) + @world.goblins.sum(&:health))
        turn -= 1
        clear_screen
        printf("%s win!\nScore:\n", @world.elves.any? ? 'Elves'.green : 'Goblins'.red)
        printf("%d * (%s)\n%d * %d = %d\n",
               turn,
               (@world.elves.map(&:health) + @world.goblins.map(&:health)).join(' + '),
               turn, hp, turn * hp)

        return turn * hp
      rescue
        elf_power += 1
      end
    end
  end

  private

  def clear_screen
    printf("%s[2J%s[H", 27.chr, 27.chr)
  end

  def parse(body, elf_power)
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

  def render(order)
    left = wrap_grid(@world.to_s)
    left_width = left.lines.map { |str| str.uncolorize.size }.max
    right = ['', 'Turn Order'] + order

    left.lines.zip(right).map do |left, right|
      w = left_width
      w += left.size - left.uncolorize.size if left
      format("%-#{w}s  %s", left.rstrip, right)
    end.join("\n")
  end

  def wrap_grid(body)
    h = w = 0
    body.lines.each do |line|
      w = [line.uncolorize.size - 1, w].max
      h += 1
    end

    w_width = (w - 1).to_s.size
    h_width = (h - 1).to_s.size
    w_fmt = format('%%%dd', w_width)
    h_fmt = format('%%%dd', h_width)

    col_nums = w.times.map { |num| format(w_fmt, num) }
    header =
      w_width.times
        .map { |n| (' ' * h_width) +  col_nums.map { |num| num[n] }.join }
    lines =
      body.lines
        .each_with_index
        .map { |line, i| format('%s%s', format(h_fmt, i), line.rstrip) }
    (header + lines).join("\n")
  end
end

puzzle = Puzzle.new(ARGV[0])
puzzle.call_1
# printf("1: %p\n2: %p\n", puzzle.call_1, puzzle.call_2)
