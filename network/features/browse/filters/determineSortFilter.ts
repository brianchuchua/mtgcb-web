interface DetermineSortFilterFunction {
  (sortBy: string, sortByDirection: 'ASC' | 'DESC'): string[];
}

const determineSortFilter: DetermineSortFilterFunction = (sortBy, sortByDirection) => {
  if (sortBy === 'collectorNumber') {
    return [`collectorNumberNumeric_${sortByDirection}`, `collectorNumber_${sortByDirection}`, 'name_ASC', 'releasedAt_ASC'];
  }
  if (sortBy === 'releasedAt') {
    return [`${sortBy}_${sortByDirection}`, 'name_ASC'];
  }
  if (sortBy === 'name') {
    return [`${sortBy}_${sortByDirection}`, 'releasedAt_ASC'];
  }

  return [`${sortBy}_${sortByDirection}`, 'name_ASC', 'releasedAt_ASC'];
};

export default determineSortFilter;
