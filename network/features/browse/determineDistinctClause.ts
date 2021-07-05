const determineDistinctClause = (showAllPrintings: boolean, sortBy: string[]): string[] => {
  if (showAllPrintings || !sortBy?.[0] || sortBy[0] === '_') {
    return null;
  }

  const sortByAttribute = sortBy[0].split('_')[0];

  return [sortByAttribute, 'name'];
};

export default determineDistinctClause;
