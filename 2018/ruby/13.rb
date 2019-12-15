#!/bin/env ruby
require 'pry-byebug'
require 'colorize'

class MapGen
  CARS = { '<' => :w, '>' => :e, 'v' => :s, '^' => :n }
  def initialize(body)
    @body = body
    @width = @body.lines.map(&:size).max
    @height = @body.lines.size
    @cells = Array.new(@width * @height)
    @cars = []
  end

  def call
    @height.times do |y|
      @width.times do |x|
        c = cell(x, y)
        c.dirs = dirs_for(x, y, c.ch)
        @cars << Car.new(cell(x, y), CARS[c.ch]) if CARS.key?(c.ch)
      end
    end

    Map.new(@cells.compact.select { |cell| cell.dirs.compact.any? }, @cars)
  end

  def char(x, y)
    @body.lines[y]&.send(:[], x)
  end

  def cell(x, y)
    @cells[y * @width + x] ||= Cell.new(x, y, char(x, y))
  end

  def dirs_for(x, y, ch)
    case ch
    when '+'
      { n: cell(x + 0, y - 1), e: cell(x + 1, y + 0),
        s: cell(x + 0, y + 1), w: cell(x - 1, y + 0) }
    when '-', '<', '>'
      { e: cell(x + 1, y + 0), w: cell(x - 1, y + 0) }
    when '|', 'v', '^'
      { n: cell(x + 0, y - 1), s: cell(x + 0, y + 1) }
    when '/'
      if %w[| + ^ v].include?(char(x, y - 1)) && %w[- + < >].include?(char(x - 1, y))
        { n: cell(x + 0, y - 1), w: cell(x - 1, y + 0) }
      else
        { e: cell(x + 1, y + 0), s: cell(x + 0, y + 1) }
      end
    when '\\'
      if %w[| + ^ v].include?(char(x, y + 1)) && %w[- + < >].include?(char(x - 1, y))
        { s: cell(x + 0, y + 1), w: cell(x - 1, y + 0) }
      else
        { n: cell(x + 0, y - 1), e: cell(x + 1, y + 0) }
      end
    else
      {}
    end
  end
end

class Map
  attr_accessor :cells, :cars

  def initialize(cells, cars)
    @cells = cells
    @cars = cars
    sort!
  end

  def sort!
    @cars.sort_by! { |c| [c.cell.y, c.cell.x] }
  end

  def tick
    @cars.each { |c| c.move }
    sort!
  end

  def crashed_cars
    cars
      .group_by { |c| [c.cell.x, c.cell.y] }
      .values
      .select { |arr| arr.size > 1 }
  end

  def crash_cells
    crashed_cars.flat_map { |arr| arr.map(&:cell) }.uniq
  end

  def to_s
    width  = @cells.map(&:x).max + 1
    height = @cells.map(&:y).max + 1

    cell_map = Hash[cells.map { |c| [[c.x, c.y], c] }]
    car_map  = Hash[cars.map { |c| [[c.cell.x, c.cell.y], c] }]

    (0...height).map do |y|
      (0...width).map do |x|
        if (cell = cell_map[[x, y]])
          if crash_cells.include?(cell)
            'X'.yellow
          elsif (car = car_map[[x, y]])
            car.to_s.red
          else
            cell.to_s
          end
        else
          ' '
        end
      end.join
    end.join("\n")
  end
end

class Cell
  attr_accessor :x, :y, :ch, :dirs

  def initialize(x, y, ch)
    @x = x
    @y = y
    @ch = ch
    @dirs = {}
  end

  def intersection?
    @dirs.size > 2
  end

  def inspect
    format('#<Cell %d,%d %p %s>', @x, @y, @ch, @dirs.keys.join(' '))
  end

  def to_s
    case dirs.keys
    when [:n, :e, :s, :w]   then '+'
    when [:n, :s]           then '|'
    when [:e, :w]           then '-'
    when [:e, :s], [:n, :w] then '/'
    when [:n, :e], [:s, :w] then '\\'
    end
  end
end

class Car
  TURNS = {
    left:     { n: :w, e: :n, s: :e, w: :s },
    straight: { n: :n, e: :e, s: :s, w: :w },
    right:    { n: :e, e: :s, s: :w, w: :n },
    back:     { n: :s, e: :w, s: :n, w: :e },
  }

  attr_accessor :cell, :dir, :turn

  def initialize(cell, dir)
    @cell = cell
    @dir = dir
    @turn = :left
  end

  def move
    if @cell.intersection?
      @dir = TURNS[@turn][@dir]
      inc_turn
    else
      @dir = @cell.dirs.keys.find { |d| d != TURNS[:back][dir] }
    end
    @cell = @cell.dirs[@dir]
  end

  def inc_turn
    opts = TURNS.keys
    @turn = opts[(opts.index(@turn) + 1) % 3]
  end

  def to_s
    case @dir
    when :n then '^'
    when :e then '>'
    when :s then 'v'
    when :w then '<'
    end
  end
end

class Puzzle
  def initialize(path)
    @map = MapGen.new(IO.read(path)).call
  end

  def call_1
    loop do
      # puts @map.to_s
      @map.tick
      crash = @map.crash_cells.first
      return [crash.x, crash.y].join(',') if crash
    end
  end

  def call_2
    i = 0
    printf "\n%s %d", @map.to_s, i
    while @map.cars.any?
      i += 1
      # puts i += 1
      # puts @map.to_s if i > 9700
      puts @map.to_s
      @map.tick

      if @map.cars.size == 1
        car = @map.cars.first
        return [car.cell.x, car.cell.y].join(',')
      end

      printf "\n%s %d", @map.to_s, i if i > 9700
      if @map.crashed_cars.any?
        printf "\n%s %d", @map.to_s, i
        # binding.pry if i > 9500
      end
      @map.crashed_cars.flatten.each { |c| @map.cars.delete(c) }
    end
  end
end

puzzle = Puzzle.new(ARGV[0])
printf("1: %p\n2: %p\n", puzzle.call_1, puzzle.call_2)
