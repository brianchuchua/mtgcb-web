import { allCards as allCardsQuery } from '../../queries';
import { api } from '../../index';
import buildBrowseFilter from './buildBrowseFilter';
import { SearchOptions } from './commonTypes';

const getAllCards = async (searchOptions: SearchOptions) => {
  const { first, skip, sortBy, name, cardTypes, cardColors } = searchOptions;

  const where = buildBrowseFilter({ cardTypes, cardColors });

  try {
    const response = await api.post('', {
      query: allCardsQuery,
      variables: {
        first,
        skip,
        sortBy,
        name,
        where,
      },
    });
    return response;
  } catch (error) {
    console.log(error);
    return null;
  }
};

export default getAllCards;
