const determineDistinctClause = (showAllPrintings: boolean, sortBy: string): string[] => {
  if (showAllPrintings || sortBy == null) {
    return null;
  }

  return [sortBy, 'name'];
};

export default determineDistinctClause;
