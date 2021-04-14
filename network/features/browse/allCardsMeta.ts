import { allCardsMeta as allCardsMetaQuery } from '../../queries';
import { api } from '../../index';
import buildBrowseFilter from './buildBrowseFilter';
import { SearchOptions } from './commonTypes';

interface GetAllCardsMetaFunction {
  (searchOptions: SearchOptions): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const getAllCardsMeta: GetAllCardsMetaFunction = async (searchOptions) => {
  const { name, cardTypes, cardColors, showAllPrintings } = searchOptions;

  const where = buildBrowseFilter({ cardTypes, cardColors });
  const distinct = showAllPrintings ? '' : 'name';

  try {
    const response = await api.post('', {
      query: allCardsMetaQuery,
      variables: {
        name,
        where,
        distinct,
      },
    });
    return response;
  } catch (error) {
    return null;
  }
};

export default getAllCardsMeta;
