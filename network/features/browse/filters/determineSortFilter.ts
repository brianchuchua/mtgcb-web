interface DetermineSortFilterFunction {
  (sortBy: string, sortByDirection: 'ASC' | 'DESC'): string[];
}

const determineSortFilter: DetermineSortFilterFunction = (sortBy, sortByDirection) => [
  `${sortBy}_${sortByDirection}`,
  'name_ASC',
  'releasedAt_ASC',
];

export default determineSortFilter;
