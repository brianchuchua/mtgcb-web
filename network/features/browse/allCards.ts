import { allCards as allCardsQuery } from '../../queries';
import { api } from '../../index';

enum SortByOptions {
  nameAsc = 'name_ASC',
  nameDesc = 'name_DESC',
}

interface SearchOptions {
  first?: number;
  skip?: number;
  sortBy?: SortByOptions;
  name?: string;
}

const getAllCards = async (searchOptions: SearchOptions) => {
  const { first, skip, sortBy, name } = searchOptions;

  try {
    const response = await api.post('', {
      query: allCardsQuery,
      variables: {
        first,
        skip,
        sortBy,
        name,
      },
    });
    return response;
  } catch (error) {
    console.log(error);
    return null;
  }
};

export default getAllCards;
