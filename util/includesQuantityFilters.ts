import { CardStatSearch } from '../features/collections/collectionSlice';

export const includesQuantityFilters = (cardStatSearches: CardStatSearch[], sortBy: string) => {
  for (const cardStatSearch of cardStatSearches) {
    const { searchAttribute, value } = cardStatSearch;
    if (value !== '' && (searchAttribute === 'cardsAll' || searchAttribute === 'cardsNormal' || searchAttribute === 'cardsFoil')) {
      return true;
    }
  }
  if (
    sortBy === 'currentValue' ||
    sortBy === 'costToComplete' ||
    sortBy === 'percentageCollected' ||
    sortBy === 'quantityAll' ||
    sortBy === 'quantityNormal' ||
    sortBy === 'quantityFoil'
  ) {
    return true;
  }

  return false;
};
