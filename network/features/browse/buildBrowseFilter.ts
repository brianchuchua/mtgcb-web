import { CardStatSearch, CardType, CardSet, CardColors } from '../../../features/browse/browseSlice';

import { addCardColorFilter, addCardTypeFilter, addCardSetFilter, addOracleTextFilter, addCardStatFilter } from './filters';

interface BuildBrowseFilterSettings {
  cardTypes?: CardType[];
  cardSets?: CardSet[];
  cardColors: CardColors;
  oracleTextQuery?: string;
  cardStatSearches?: CardStatSearch[];
}

interface BuildBrowseFilterFunction {
  (filterSettings: BuildBrowseFilterSettings): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const buildBrowseFilter: BuildBrowseFilterFunction = ({ cardTypes, cardSets, cardColors, oracleTextQuery, cardStatSearches }) => {
  const where = { AND: [] };

  addCardTypeFilter(cardTypes, where);
  addCardSetFilter(cardSets, where);
  addCardColorFilter(cardColors, where);
  addOracleTextFilter(oracleTextQuery, where);
  addCardStatFilter(cardStatSearches, where);

  return where;
};

export default buildBrowseFilter;
