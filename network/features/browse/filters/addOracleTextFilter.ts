interface AddOracleTextFilterFunction {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (oracleTextSearchQuery: string, where: { AND: any[] }): any;
}

const addOracleTextFilter: AddOracleTextFilterFunction = (oracleTextSearchQuery, where) => {
  if (oracleTextSearchQuery) {
    const REGEX_TO_SPLIT_BY_QUOTED_STRINGS_AND_SPACES = / +(?=(?:(?:[^"]*"){2})*[^"]*$)/g;
    const REGEX_TO_REMOVE_LEADING_AND_TRAILING_QUOTES = /^"(.*)"$/;
    const oracleTextQueries = oracleTextSearchQuery.split(REGEX_TO_SPLIT_BY_QUOTED_STRINGS_AND_SPACES);

    const graphqlFilters = { AND: [] };
    for (const oracleTextQuery of oracleTextQueries) {
      const oracleTextQueryWithWrappingQuotesRemoved = oracleTextQuery.replace(REGEX_TO_REMOVE_LEADING_AND_TRAILING_QUOTES, '$1');
      graphqlFilters.AND.push({ oracleText_contains_i: oracleTextQueryWithWrappingQuotesRemoved });
    }
    where.AND.push(graphqlFilters);
    console.log(graphqlFilters);
  }
};

export default addOracleTextFilter;
