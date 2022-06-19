interface DetermineAdditionalSortFilterFunction {
  (sortBy: string, sortByDirection: 'ASC' | 'DESC'): string;
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
    return `${sortBy}_${sortByDirection}`;
  }
  return null;
};

export default determineAdditionalSortFilter;
