import { api } from '../../index';
import { allSets as allSetsQuery } from '../../queries';
import { SetSearchOptions } from './commonTypes';

interface GetAllSetsFunction {
  (searchOptions: SetSearchOptions): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const getAllSets: GetAllSetsFunction = async (searchOptions) => {
  const { first, skip } = searchOptions;

  try {
    const response = await api.post('', {
      query: allSetsQuery,
      variables: {
        sortBy: 'releasedAt_DESC',
        first,
        skip,
      },
    });
    return response;
  } catch (error) {
    console.log(error);
    return null;
  }
};

export default getAllSets;
