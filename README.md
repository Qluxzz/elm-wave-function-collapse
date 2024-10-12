# Elm wave function collapse

Inspired by the sudoku example in this youtube video [Superpositions, Sudoku, The Wave Function Collapse algorithm](https://www.youtube.com/watch?v=2SuvO4Gi7uY)

An interactive example of how the wave function collapse algorithm works. When solving, it finds the cell(s) in the sudoku board with the least amount of entropy (the fewest alternatives to select from), and selects a random alternative for the cell. It continues doing this until we've either completed the entire sudoku board or we get stuck, where we then just reset the board to the initial configuration and try to solve it once again.

Built using [Elm](https://elm-lang.org/)
