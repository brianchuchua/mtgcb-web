/* eslint-disable import/no-cycle */
import debounce from 'lodash.debounce';
import qs from 'qs';
import { CardColors, CardRarity, CardSet, CardType, PriceTypes, SetCategory, SetType } from '../features/browse/browseSlice';
import { SetCompletionStatus } from '../features/collections/collectionSlice';
import { CardStatSearch } from '../features/sets/setSlice';

interface QueryParams {
  card?: string;
  oracle?: string;
  artist?: string;
  types?: string;
  sets?: string;
  rarities?: 'common' | 'uncommon' | 'rare' | 'mythic' | 'special' | 'none';
  colors?: string;
  stats?: string;
  sort?: string;
  order?: 'asc' | 'desc';
  view?: 'cards' | 'sets';
  price?: PriceTypes;
  mode?: 'table' | 'grid';
  set?: string;
  setSort: string;
  setOrder: 'asc' | 'desc';
  setCats: string;
  setTypes: string;
  status?: SetCompletionStatus;
}

let queryFromUrl: QueryParams | Record<string, never> = {};

if (typeof window !== 'undefined') {
  queryFromUrl = qs.parse(window?.location?.search || '', { ignoreQueryPrefix: true });
}

export const computedQueryFromUrl = queryFromUrl;

type QueryLabel = keyof QueryParams;

export const updateSearchInUrlQuery = (queryLabel: QueryLabel, newValue: string | string[] | boolean): void => {
  if (typeof window !== 'undefined') {
    const query = qs.parse(window?.location?.search || '', { ignoreQueryPrefix: true });
    query[queryLabel] = newValue;
    if (newValue === null || newValue === '' || (Array.isArray(newValue) && newValue?.length === 0) || typeof newValue === 'undefined') {
      delete query[queryLabel];
    }
    const queryString = qs.stringify(query, { addQueryPrefix: true });

    window.history.replaceState(null, null, queryString?.length ? queryString : window.location.href.split('?')[0]);
  }
};

export const updateSearchInUrl = debounce(updateSearchInUrlQuery, 500);

export const convertCardTypesToString = (cardTypes: CardType[]): string =>
  cardTypes.map((cardType) => `${cardType.value}:${cardType.exclude ? 0 : 1}`).join(',');

export const convertStringToCardTypes = (cardTypesString: string): CardType[] => {
  if (cardTypesString === '') {
    return [];
  }
  return cardTypesString?.split(',').map((cardTypeString) => {
    const [cardType, exclude] = cardTypeString.split(':');
    return {
      category: 'Card Types',
      label: cardType,
      value: cardType,
      exclude: exclude === '0',
    };
  });
};
export const convertSetsToString = (sets: CardSet[]): string =>
  sets.map((set) => `${set.label}:${set.value}:${set.exclude ? 0 : 1}`).join(',');

export const convertStringToSets = (setsString: string): SetType[] => {
  if (setsString === '') {
    return [];
  }
  return setsString?.split(',').map((setString) => {
    const [setLabel, setValue, exclude] = setString.split(':');
    return {
      category: 'Sets',
      label: setLabel,
      value: setValue,
      exclude: exclude === '0',
    };
  });
};
export const convertRaritiesToString = (rarities: CardRarity[]): string =>
  rarities.map((rarity) => `${rarity.value}:${rarity.exclude ? 0 : 1}`).join(',');

export const convertStringToRarities = (
  raritiesString: 'common' | 'uncommon' | 'rare' | 'mythic' | 'special' | 'none' | ''
): CardRarity[] => {
  if (raritiesString === '') {
    return [];
  }
  return raritiesString?.split(',').map((rarityString) => {
    const [rarityValue, exclude] = rarityString.split(':');
    return {
      category: 'Rarities',
      value: rarityValue,
      label: rarityValue.charAt(0).toUpperCase() + rarityValue.slice(1),
      exclude: exclude === '0',
    } as CardRarity;
  });
};

export const convertColorsToString = (colors: CardColors): string => {
  const colorsString = Object.keys(colors)
    .filter((colorAttribute) => colors[colorAttribute])
    .map((colorAttribute) =>
      colorAttribute !== 'type' ? `${colorAttribute}:${colors[colorAttribute] ? 1 : 0}` : `${colorAttribute}:${colors[colorAttribute]}`
    )
    .join(',');
  return colorsString;
};

export const convertStringToColors = (colorsString: string): CardColors | Record<string, never> => {
  if (colorsString === '') {
    return {};
  }
  const colors = colorsString?.split(',').reduce((acc, colorString) => {
    const [colorAttribute, value] = colorString.split(':');
    if (colorAttribute === 'type') {
      acc[colorAttribute] = value;
    } else {
      acc[colorAttribute] = value === '1';
    }
    return acc;
  }, {});
  return colors as CardColors;
};

export const convertCardStatSearchesToString = (cardStatSearches: CardStatSearch[]): string =>
  cardStatSearches
    .map((cardStatSearch) => `${cardStatSearch.searchAttribute}:${cardStatSearch.comparator}:${cardStatSearch.value}`)
    .join(',');

export const convertStringToCardStatSearches = (cardStatSearchesString: string): CardStatSearch[] => {
  if (cardStatSearchesString === '') {
    return [];
  }
  return cardStatSearchesString?.split(',').map((cardStatSearchString) => {
    const [searchAttribute, comparator, value] = cardStatSearchString.split(':');
    return {
      searchAttribute,
      comparator,
      value,
    } as CardStatSearch;
  });
};
export const convertExpansionCategoriesToString = (expansionCategories: SetCategory[]): string =>
  expansionCategories.map((expansionCategory) => `${expansionCategory.value}:${expansionCategory.exclude ? 0 : 1}`).join(',');

export const convertStringToExpansionCategories = (expansionCategoriesString: string): SetCategory[] => {
  if (expansionCategoriesString === '') {
    return [];
  }
  return expansionCategoriesString?.split(',').map((expansionCategoryString) => {
    const [expansionCategoryValue, exclude] = expansionCategoryString.split(':');
    return {
      category: 'Set Categories',
      value: expansionCategoryValue,
      label: expansionCategoryValue,
      exclude: exclude === '0',
    };
  });
};

export const convertSetTypesToString = (setTypes: SetType[]): string =>
  setTypes.map((setType) => `${setType.label}:${setType.value}:${setType.exclude ? 0 : 1}`).join(',');

export const convertStringToSetTypes = (setTypesString: string): SetType[] => {
  if (setTypesString === '') {
    return [];
  }
  return setTypesString?.split(',').map((setTypeString) => {
    const [setTypeLabel, setTypeValue, exclude] = setTypeString.split(':');
    return {
      category: 'Set Type',
      label: setTypeLabel,
      value: setTypeValue,
      exclude: exclude === '0',
    };
  });
};

export const convertSetCompletionStatusesToString = (setCompletionStatuses: SetCompletionStatus[]): string => {
  const setCompletionStatusesString = setCompletionStatuses
    .map((setCompletionStatus) => `${setCompletionStatus}:${setCompletionStatus === 'all' ? 1 : 0}`)
    .join(',');
  return setCompletionStatusesString;
};

export const convertStringToSetCompletionStatuses = (setCompletionStatusesString: string): SetCompletionStatus[] => {
  if (setCompletionStatusesString === '') {
    return ['all'];
  }
  return setCompletionStatusesString?.split(',').map((setCompletionStatusString) => {
    const [setCompletionStatus, value] = setCompletionStatusString.split(':');
    return setCompletionStatus as SetCompletionStatus;
  });
};
