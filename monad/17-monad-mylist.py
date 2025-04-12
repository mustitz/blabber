def pure(x):
    return [x]

def bind(lst, f):
    result = []
    for x in lst:
        result = result + f(x)
    return result

BOARD_SIZE = 5

def all_ranks():
    return list(range(1, BOARD_SIZE + 1))

def connected(square1, square2):
    file1, rank1 = square1
    file2, rank2 = square2

    same_file = file1 == file2
    same_rank = rank1 == rank2
    same_diagonal = abs(file1 - file2) == abs(rank1 - rank2)

    return same_file or same_rank or same_diagonal

def unsafe(square, queens):
    return any(connected(square, queen) for queen in queens)

def add_queen(file, existing_queens):
    def check_and_add(rank):
        if unsafe((file, rank), existing_queens):
            return []
        else:
            return pure([(file, rank)] + existing_queens)

    return bind(all_ranks(), check_and_add)

def queens(n):
    if n == 0:
        return pure([])
    return bind(queens(n-1), lambda qs: add_queen(n, qs))

def print_solutions(solutions):
    for i, solution in enumerate(solutions, 1):
        print(f"Solution {i}: {solution}")

def main():
    print(f"{BOARD_SIZE}-Queens solutions:")
    solutions = queens(BOARD_SIZE)
    print_solutions(solutions)

if __name__ == "__main__":
    main()
