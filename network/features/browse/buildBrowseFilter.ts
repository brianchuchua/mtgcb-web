import { CardColors, CardRarity, CardSet, CardStatSearch, CardType } from '../../../features/browse/browseSlice';
import {
  addAdditionalCardStatFilter,
  addArtistFilter,
  addCardColorFilter,
  addCardRarityFilter,
  addCardSetFilter,
  addCardStatFilter,
  addCardTypeFilter,
  addOracleTextFilter,
} from './filters';

interface BuildBrowseFilterSettings {
  name?: string;
  cardTypes?: CardType[];
  cardSets?: CardSet[];
  cardRarities?: CardRarity[];
  cardColors?: CardColors;
  oracleTextQuery?: string;
  artistQuery?: string;
  cardStatSearches?: CardStatSearch[];
  orderBy: string;
}

interface BuildAdditionalWhereFilterFunctionSettings {
  name?: string;
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
  name,
  cardTypes,
  cardSets,
  cardRarities,
  cardColors,
  oracleTextQuery,
  artistQuery,
  cardStatSearches,
  orderBy,
}) => {
  const where = { AND: [] };
  if (name) {
    where.AND.push({
      name: {
        contains: name,
        mode: 'insensitive',
      },
    });
  }
  addCardTypeFilter(cardTypes, where);
  addCardSetFilter(cardSets, where);
  addCardRarityFilter(cardRarities, where);
  addCardColorFilter(cardColors, where);
  addOracleTextFilter(oracleTextQuery, where);
  addArtistFilter(artistQuery, where);
  addCardStatFilter(cardStatSearches, where);

  const orderByOptionsThatMayBeNull = [
    'releasedAt',
    'collectorNumber',
    'rarityNumeric',
    'convertedManaCost',
    'powerNumeric',
    'toughnessNumeric',
    'loyaltyNumeric',
    'market',
    'low',
    'average',
    'high',
    'foil',
  ];

  if (orderBy) {
    if (orderByOptionsThatMayBeNull.includes(orderBy)) {
      where.AND.push({
        [orderBy]: {
          not: null,
        },
      });
    }
  }

  return where;
};

interface BuildAdditionalWhereFilterFunction {
  (filterSettings: BuildAdditionalWhereFilterFunctionSettings): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

export const buildAdditionalWhereFilter: BuildAdditionalWhereFilterFunction = ({ cardStatSearches }) => {
  const additionalWhere = { AND: [] };

  addAdditionalCardStatFilter(cardStatSearches, additionalWhere);

  return additionalWhere;
};
export default buildBrowseFilter;
