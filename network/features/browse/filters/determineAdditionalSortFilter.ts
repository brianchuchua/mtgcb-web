interface DetermineAdditionalSortFilterFunction {
  (sortBy: string, sortByDirection: 'ASC' | 'DESC'): string;
}

const determineAdditionalSortFilter: DetermineAdditionalSortFilterFunction = (sortBy, sortByDirection) => {
  if (sortBy === 'currentValue' || sortBy === 'costToComplete' || sortBy === 'percentageCollected') {
    return `${sortBy}_${sortByDirection}`;
  }
  return null;
};

export default determineAdditionalSortFilter;
