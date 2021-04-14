import { cardTypes as cardTypesQuery } from '../../queries';
import { api } from '../../index';

interface GetCardTypesFunction {
  (): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const getCardTypes: GetCardTypesFunction = async () => {
  try {
    const response = await api.post('', {
      query: cardTypesQuery,
    });
    return response;
  } catch (error) {
    return null;
  }
};

export default getCardTypes;
