def sort(nums):
    for j in range(1, len(nums)):
        curnum = nums[j]
        i = j - 1
        while i >= 0 and nums[i] > curnum:
            nums[i + 1] = nums[i]
            i = i - 1
        nums[i + 1] = curnum
    return nums

def test():
    assert [1, 2, 3, 4, 5, 6] == sort([5, 2, 4, 6, 1, 3])

    print 'test ok'

if __name__ == '__main__':
    test()