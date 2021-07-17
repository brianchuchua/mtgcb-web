import { CardStatSearch, CardType, CardSet, CardRarity, CardColors } from '../../../features/browse/browseSlice';

import {
  addCardColorFilter,
  addCardTypeFilter,
  addCardSetFilter,
  addCardRarityFilter,
  addOracleTextFilter,
  addCardStatFilter,
} from './filters';

interface BuildBrowseFilterSettings {
  cardTypes?: CardType[];
  cardSets?: CardSet[];
  cardRarities?: CardRarity[];
  cardColors: CardColors;
  oracleTextQuery?: string;
  cardStatSearches?: CardStatSearch[];
}

interface BuildBrowseFilterFunction {
  (filterSettings: BuildBrowseFilterSettings): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const buildBrowseFilter: BuildBrowseFilterFunction = ({
  cardTypes,
  cardSets,
  cardRarities,
  cardColors,
  oracleTextQuery,
  cardStatSearches,
}) => {
  const where = { AND: [] };

  addCardTypeFilter(cardTypes, where);
  addCardSetFilter(cardSets, where);
  addCardRarityFilter(cardRarities, where);
  addCardColorFilter(cardColors, where);
  addOracleTextFilter(oracleTextQuery, where);
  addCardStatFilter(cardStatSearches, where);

  return where;
};

export default buildBrowseFilter;
