import { allCardsMeta as allCardsMetaQuery } from '../../queries';
import { api } from '../../index';

interface SearchOptions {
  name?: string;
  first?: number;
  skip?: number;
}

const getAllCardsMeta = async (searchOptions: SearchOptions) => {
  const { name } = searchOptions;

  try {
    const response = await api.post('', {
      query: allCardsMetaQuery,
      variables: {
        name,
      },
    });
    return response;
  } catch (error) {
    return null;
  }
};

export default getAllCardsMeta;
