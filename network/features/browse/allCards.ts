import { allCards as allCardsQuery } from '../../queries';
import { api } from '../../index';
import buildBrowseFilter from './buildBrowseFilter';
import { SearchOptions } from './commonTypes';
import determineDistinctClause from './determineDistinctClause';

interface GetAllCardsFunction {
  (searchOptions: SearchOptions): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const getAllCards: GetAllCardsFunction = async (searchOptions) => {
  const { first, skip, sortBy, name, oracleTextQuery, cardSets, cardTypes, cardColors, showAllPrintings, cardStatSearches } = searchOptions;

  const where = buildBrowseFilter({ cardSets, cardTypes, cardColors, oracleTextQuery, cardStatSearches });
  const distinct = determineDistinctClause(showAllPrintings, sortBy);

  try {
    const response = await api.post('', {
      query: allCardsQuery,
      variables: {
        first,
        skip,
        sortBy,
        name,
        where,
        distinct,
      },
    });
    return response;
  } catch (error) {
    console.log(error);
    return null;
  }
};

export default getAllCards;
