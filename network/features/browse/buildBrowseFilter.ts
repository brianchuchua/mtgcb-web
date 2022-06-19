import { CardColors, CardRarity, CardSet, CardStatSearch, CardType } from '../../../features/browse/browseSlice';
import {
  addAdditionalCardStatFilter,
  addCardColorFilter,
  addCardRarityFilter,
  addCardSetFilter,
  addCardStatFilter,
  addCardTypeFilter,
  addOracleTextFilter,
} from './filters';

interface BuildBrowseFilterSettings {
  cardTypes?: CardType[];
  cardSets?: CardSet[];
  cardRarities?: CardRarity[];
  cardColors?: CardColors;
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

interface BuildAdditionalWhereFilterFunction {
  (filterSettings: BuildBrowseFilterSettings): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

export const buildAdditionalWhereFilter: BuildAdditionalWhereFilterFunction = ({ cardStatSearches }) => {
  const additionalWhere = { AND: [] };

  addAdditionalCardStatFilter(cardStatSearches, additionalWhere);

  return additionalWhere;
};
export default buildBrowseFilter;
