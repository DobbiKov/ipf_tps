def circular_hanoi(n, source, target, auxiliary, pegs):
    # Base case: when there's only one disk to move
    if n == 1:
        # Check if move from source to target is allowed directly
        if (source % 3) + 1 == target:
            move_disk(source, target, pegs)
        else:
            # Move disk via the next peg in the circle
            next_peg = (source % 3) + 1
            move_disk(source, next_peg, pegs)
            move_disk(next_peg, target, pegs)
        return

    # Check if move from source to target is allowed directly
    if (source % 3) + 1 == target:
        # Move n-1 disks to auxiliary peg
        circular_hanoi(n - 1, source, auxiliary, target, pegs)
        # Move the largest disk to target peg
        move_disk(source, target, pegs)
        # Move the n-1 disks from auxiliary peg to target peg
        circular_hanoi(n - 1, auxiliary, target, source, pegs)
    else:
        # Move n-1 disks to target peg (using auxiliary peg)
        circular_hanoi(n - 1, source, target, auxiliary, pegs)
        # Move the largest disk to the next peg in the circle
        next_peg = (source % 3) + 1
        move_disk(source, next_peg, pegs)
        # Move n-1 disks back to source peg (using auxiliary peg)
        circular_hanoi(n - 1, target, source, auxiliary, pegs)
        # Move the largest disk to the target peg
        move_disk(next_peg, target, pegs)
        # Move n-1 disks to target peg
        circular_hanoi(n - 1, source, target, auxiliary, pegs)

def move_disk(source, target, pegs):
    """
    Moves the top disk from the source peg to the target peg and prints the move and current state.

    Parameters:
    - source: the source peg number
    - target: the target peg number
    - pegs: dictionary representing the current state of pegs
    """
    if not pegs[source]:
        raise Exception(f"No disks to move from peg {source}")

    disk = pegs[source].pop()
    pegs[target].append(disk)
    print(f"Move disk {disk} from peg {source} to peg {target}")
    print_state(pegs)

def print_state(pegs):
    """
    Prints the current state of all pegs.

    Parameters:
    - pegs: dictionary representing the current state of pegs
    """
    # Define peg names for visualization
    peg_names = {1: "start", 2: "mid", 3: "end"}
    for peg in [1, 2, 3]:
        if pegs[peg]:
            # Join disk numbers with '-' to represent stack from bottom to top
            disks = '-'.join(map(str, pegs[peg]))
        else:
            disks = '-'
        print(f"{peg_names[peg]}:{disks}")
    print()  # Add an empty line for better readability

def initialize_pegs(n):
    """
    Initializes the pegs with all disks on the source peg.

    Parameters:
    - n: number of disks

    Returns:
    - pegs: dictionary representing the initialized state of pegs
    """
    return {
        1: list(range(n, 0, -1)),  # Peg 1 starts with all disks, largest at bottom
        2: [],                      # Peg 2 is empty
        3: []                       # Peg 3 is empty
    }

def main():
    """
    Main function to execute the Circular Towers of Hanoi with visualization.
    """
    n_disks = 3  # Change this value for more disks
    pegs = initialize_pegs(n_disks)
    
    print("Initial State:")
    print_state(pegs)
    
    circular_hanoi(n_disks, 1, 3, 2, pegs)
    
    print("Final State:")
    print_state(pegs)

if __name__ == "__main__":
    main()
