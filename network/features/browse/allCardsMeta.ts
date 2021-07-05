import { allCardsMeta as allCardsMetaQuery } from '../../queries';
import { api } from '../../index';
import buildBrowseFilter from './buildBrowseFilter';
import { SearchOptions } from './commonTypes';
import determineDistinctClause from './determineDistinctClause';

interface GetAllCardsMetaFunction {
  (searchOptions: SearchOptions): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const getAllCardsMeta: GetAllCardsMetaFunction = async (searchOptions) => {
  const { name, oracleTextQuery, cardSets, cardTypes, cardColors, showAllPrintings, cardStatSearches, sortBy } = searchOptions;

  const where = buildBrowseFilter({ cardSets, cardTypes, cardColors, oracleTextQuery, cardStatSearches });
  const distinct = determineDistinctClause(showAllPrintings, sortBy);

  try {
    const response = await api.post('', {
      query: allCardsMetaQuery,
      variables: {
        name,
        where,
        sortBy,
        distinct,
      },
    });
    return response;
  } catch (error) {
    return null;
  }
};

export default getAllCardsMeta;
