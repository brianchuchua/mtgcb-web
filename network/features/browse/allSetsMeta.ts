import { api } from '../../index';
import { allSetsMeta as allSetsMetaQuery } from '../../queries';

interface GetAllSetsMetaFunction {
  (): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const getAllSetsMeta: GetAllSetsMetaFunction = async () => {
  try {
    const response = await api.post('', {
      query: allSetsMetaQuery,
      variables: {
        sortBy: 'releasedAt_DESC',
      },
    });
    return response;
  } catch (error) {
    console.log(error);
    return null;
  }
};

export default getAllSetsMeta;
