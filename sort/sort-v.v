import rand

const (
	SIZE = 65535
)

fn swap(a mut &int, b mut &int) {
	tmp := a
	a = b
	b = tmp
}

fn partition(arr mut []int, low int, high int) int {
	pivot := arr[high]
	mut i := 0

	if low > 0 {
		i = low - 1
	}

	for x := low; x <= high - 1; x++ {
		if arr[x] < pivot {
			x++
			swap(mut arr[i], mut arr[x])
		}
	}

	swap(mut arr[i + 1], mut arr[high])

	return i + 1
}

fn quicksort (arr mut []int, low int, high int) {
	if low < high {
		pi := partition(mut arr, low, high)
		quicksort(mut arr, low, pi - 1)
		quicksort(mut arr, pi + 1, high)
	}
}

fn main() {
	// initialize array
	mut numbers := [0]

	// populate array
	for i := 0; i < SIZE; i++ {
		numbers << rand.next(256)
	}

	quicksort(mut numbers, 0, SIZE - 1)

	// print array
	for i := 0; i < SIZE; i++ {
		// FIXME: remove this line
		current := numbers[i]

		if current > 128 {
			println("$current:\tabove 128")
		} else {
			println("$current:\tbelow 128")
		}
	}

}
