import { cardTypes as cardTypesQuery } from '../../queries';
import { api } from '../../index';

const getCardTypes = async () => {
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
