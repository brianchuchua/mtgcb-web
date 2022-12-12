interface DetermineAdditionalSortFilterFunction {
  (sortBy: string, sortByDirection: 'asc' | 'desc'): string;
}

const determineAdditionalSortFilter: DetermineAdditionalSortFilterFunction = (sortBy, sortByDirection) => {
  if (
    sortBy === 'currentValue' ||
    sortBy === 'costToComplete' ||
    sortBy === 'percentageCollected' ||
    sortBy === 'quantityAll' ||
    sortBy === 'quantityNormal' ||
    sortBy === 'quantityFoil'
  ) {
    return `${sortBy}_${sortByDirection.toLowerCase()}`;
  }
  return null;
};

export default determineAdditionalSortFilter;
