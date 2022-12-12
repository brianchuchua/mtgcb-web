interface DetermineSortFilterFunction {
  (sortBy: string, sortByDirection: 'asc' | 'desc'): any;
}

const determineSortFilter: DetermineSortFilterFunction = (sortBy, sortByDirection) => {
  if (
    sortBy === 'currentValue' ||
    sortBy === 'costToComplete' ||
    sortBy === 'percentageCollected' ||
    sortBy === 'quantityAll' ||
    sortBy === 'quantityNormal' ||
    sortBy === 'quantityFoil'
  ) {
    return [{ name: 'asc' }];
  }
  // eslint-disable-next-line no-param-reassign
  sortByDirection = sortByDirection?.toLowerCase() as 'asc' | 'desc';

  if (sortBy === 'collectorNumber') {
    return [{ collectorNumberNumeric: sortByDirection }, { collectorNumber: sortByDirection }, { name: 'asc' }, { releasedAt: 'asc' }];
  }
  if (sortBy === 'releasedAt') {
    return [{ releasedAt: sortByDirection }, { name: 'asc' }];
  }
  if (sortBy === 'name') {
    return [{ name: sortByDirection }, { releasedAt: 'asc' }];
  }

  return [{ [sortBy]: sortByDirection }, { name: 'asc' }, { releasedAt: 'asc' }];
};

export default determineSortFilter;
