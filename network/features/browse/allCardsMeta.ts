import { allCardsMeta as allCardsMetaQuery } from '../../queries';
import { api } from '../../index';
import buildBrowseFilter from './buildBrowseFilter';
import { SearchOptions } from './commonTypes';

const getAllCardsMeta = async (searchOptions: SearchOptions) => {
  const { name, cardTypes } = searchOptions;

  const where = buildBrowseFilter({ cardTypes });

  try {
    const response = await api.post('', {
      query: allCardsMetaQuery,
      variables: {
        name,
        where,
      },
    });
    return response;
  } catch (error) {
    return null;
  }
};

export default getAllCardsMeta;
