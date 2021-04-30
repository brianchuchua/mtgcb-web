import { allSets as allSetsQuery } from '../../queries';
import { api } from '../../index';

interface GetAllSetsFunction {
  (): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const getAllSets: GetAllSetsFunction = async () => {
  try {
    const response = await api.post('', {
      query: allSetsQuery,
      variables: {
        sortBy: 'name_ASC',
      },
    });
    return response;
  } catch (error) {
    console.log(error);
    return null;
  }
};

export default getAllSets;
