import { createSlice, PayloadAction } from '@reduxjs/toolkit';
// eslint-disable-next-line import/no-cycle
import {
  computedQueryFromUrl as queryFromUrl,
  convertCardStatSearchesToString,
  convertCardTypesToString,
  convertColorsToString,
  convertRaritiesToString,
  convertStringToCardStatSearches,
  convertStringToCardTypes,
  convertStringToColors,
  convertStringToRarities,
  updateSearchInUrl,
} from '../../../util/queryStringMappers';
import { getValueFromLocalStorage, setValueToLocalStorage } from '../../../util/useLocalStorage';

const viewModeFromLocalStorage = getValueFromLocalStorage('viewMode', 'grid');
const priceTypeFromLocalStorage = getValueFromLocalStorage('priceType', 'market');

const initialState: SetState = {
  searchQuery: queryFromUrl.card || '',
  oracleTextQuery: queryFromUrl.oracle || '',
  cardTypes: convertStringToCardTypes(queryFromUrl.types) || [],
  cardRarities: convertStringToRarities(queryFromUrl.rarities) || [],
  cardColors: (convertStringToColors(queryFromUrl.colors) as CardColors) || {
    white: false,
    blue: false,
    black: false,
    red: false,
    green: false,
    colorless: false,
    type: 'at-least-these-colors',
  },
  showAllPrintings: true,
  cardStatSearches: (convertStringToCardStatSearches(queryFromUrl.stats) as CardStatSearch[]) || [
    { searchAttribute: 'convertedManaCost', comparator: 'gt', value: '' },
  ],
  sortBy: queryFromUrl.sort || 'collectorNumber',
  sortByDirection: queryFromUrl.order || 'asc',
  isFormVisible: false,
  viewSubject: 'cards',
  viewMode: queryFromUrl.mode || viewModeFromLocalStorage || 'grid',
  priceType: queryFromUrl.price || priceTypeFromLocalStorage || 'market',
};

const emptyState = {
  searchQuery: '',
  oracleTextQuery: '',
  cardTypes: [],
  cardSets: [],
  cardRarities: [],
  cardColors: {
    white: false,
    blue: false,
    black: false,
    red: false,
    green: false,
    colorless: false,
    type: 'at-least-these-colors',
  },
  showAllPrintings: true,
  cardStatSearches: [{ searchAttribute: 'convertedManaCost', comparator: 'gt', value: '' }],
  sortBy: 'collectorNumber',
  sortByDirection: 'asc',
  isFormVisible: true,
};

const setCollectionSlice = createSlice({
  name: 'setCollection',
  initialState,
  reducers: {
    reset: (state) => {
      if (typeof window !== 'undefined') {
        window.history.replaceState(null, null, window.location.href.split('?')[0]);
        window.scrollTo(0, 0);
      }
      return { ...state, ...emptyState } as SetState;
    },
    setSearchQuery(state, action: PayloadAction<SearchQuery>) {
      const { searchQuery } = action.payload;
      state.searchQuery = searchQuery;
      updateSearchInUrl('card', searchQuery);
    },
    setOracleTextQuery(state, action: PayloadAction<OracleTextQuery>) {
      const { oracleTextQuery } = action.payload;
      state.oracleTextQuery = oracleTextQuery;
      updateSearchInUrl('oracle', oracleTextQuery);
    },
    setCardTypes(state, action: PayloadAction<CardTypes>) {
      const { cardTypes } = action.payload;
      state.cardTypes = cardTypes;
      updateSearchInUrl('types', convertCardTypesToString(cardTypes));
    },
    setCardRarities(state, action: PayloadAction<CardRarities>) {
      const { cardRarities } = action.payload;
      state.cardRarities = cardRarities;
      updateSearchInUrl('rarities', convertRaritiesToString(cardRarities));
    },
    setCardColors(state, action: PayloadAction<string>) {
      const color = action.payload;
      state.cardColors[color] = !state.cardColors[color];
      const newCardColors = { ...state.cardColors };
      updateSearchInUrl('colors', convertColorsToString(newCardColors));
    },
    setColorType(state, action: PayloadAction<ColorTypes>) {
      const type = action.payload;
      state.cardColors.type = type;
      const newCardColors = { ...state.cardColors };
      newCardColors.type = type;
      updateSearchInUrl('colors', convertColorsToString(newCardColors));
    },
    setShowAllPrintings(state, action: PayloadAction<boolean>) {
      const showAllPrintings = action.payload;
      state.showAllPrintings = showAllPrintings;
    },
    setSearchAttribute(state, action: PayloadAction<SearchAttributeChangePayload>) {
      const { searchAttribute, index } = action.payload;
      state.cardStatSearches[index].searchAttribute = searchAttribute;
      const newCardStatSearches = [...state.cardStatSearches];
      newCardStatSearches[index].searchAttribute = searchAttribute;
      updateSearchInUrl('stats', convertCardStatSearchesToString(newCardStatSearches));
    },
    setComparator(state, action: PayloadAction<SearchComparatorPayload>) {
      const { comparator, index } = action.payload;
      state.cardStatSearches[index].comparator = comparator;
      const newCardStatSearches = [...state.cardStatSearches];
      newCardStatSearches[index].comparator = comparator;
      updateSearchInUrl('stats', convertCardStatSearchesToString(newCardStatSearches));
    },
    setCardStatSearches(state, action: PayloadAction<SearchValuePayload>) {
      const { value, index } = action.payload;
      state.cardStatSearches[index].value = value;
      const newCardStatSearches = [...state.cardStatSearches];
      newCardStatSearches[index].value = value;
      updateSearchInUrl('stats', convertCardStatSearchesToString(newCardStatSearches));
    },
    addCardStatSearch(state) {
      state.cardStatSearches.push({ searchAttribute: 'convertedManaCost', comparator: 'gt', value: '' });
      const newCardStatSearches = [...state.cardStatSearches];
      newCardStatSearches.push({
        searchAttribute: 'convertedManaCost',
        comparator: 'gt',
        value: '',
      });
      updateSearchInUrl('stats', convertCardStatSearchesToString(newCardStatSearches));
    },
    removeCardStatSearch(state) {
      if (state.cardStatSearches.length > 1) {
        state.cardStatSearches.pop();
        const newCardStatSearches = [...state.cardStatSearches];
        newCardStatSearches.pop();
        updateSearchInUrl('stats', convertCardStatSearchesToString(newCardStatSearches));
      }
    },
    setCardSort(state, action: PayloadAction<string>) {
      const sortBy = action.payload;
      state.sortBy = sortBy;
      updateSearchInUrl('sort', sortBy);
    },
    setCardSortDirection(state, action: PayloadAction<'asc' | 'desc'>) {
      const sortByDirection = action.payload;
      state.sortByDirection = sortByDirection;
      updateSearchInUrl('order', sortByDirection);
    },
    setFormVisibility(state, action: PayloadAction<FormVisibility>) {
      const { isFormVisibile } = action.payload;
      state.isFormVisible = isFormVisibile;
    },
    setViewSubject(state, action: PayloadAction<'cards' | 'sets'>) {
      const viewSubject = action.payload;
      state.viewSubject = viewSubject;
    },
    setViewMode(state, action: PayloadAction<'grid' | 'table'>) {
      const viewMode = action.payload;
      state.viewMode = viewMode;
      updateSearchInUrl('mode', viewMode);
      setValueToLocalStorage('viewMode', viewMode);
    },
    setPriceType(state, action: PayloadAction<PriceTypes>) {
      const priceType = action.payload;
      state.priceType = priceType;
      updateSearchInUrl('price', priceType);
      setValueToLocalStorage('priceType', priceType);
    },
  },
});

export const {
  setSearchQuery,
  setOracleTextQuery,
  setCardTypes,
  setCardRarities,
  setCardColors,
  setColorType,
  setShowAllPrintings,
  setSearchAttribute,
  setComparator,
  setCardStatSearches,
  addCardStatSearch,
  removeCardStatSearch,
  setCardSort,
  setCardSortDirection,
  setFormVisibility,
  setViewSubject,
  setViewMode,
  setPriceType,
  reset,
} = setCollectionSlice.actions;

export const searchAttributeOptions = [
  { value: 'convertedManaCost', label: 'CMC' },
  { value: 'powerNumeric', label: 'Power' },
  { value: 'toughnessNumeric', label: 'Toughness' },
  { value: 'loyaltyNumeric', label: 'Loyalty' },
  { value: 'market', label: 'Price (Market)' },
  { value: 'low', label: 'Price (Low)' },
  { value: 'average', label: 'Price (Average)' },
  { value: 'high', label: 'Price (High)' },
  { value: 'foil', label: 'Price (Foil)' },
  { value: 'cardsAll', label: 'Quantity (All)' },
  { value: 'cardsNormal', label: 'Quantity (Normal)' },
  { value: 'cardsFoil', label: 'Quantity (Foil)' },
];

export const sortByOptions = [
  { value: 'name', label: 'Name' },
  { value: 'releasedAt', label: 'Release Date' },
  { value: 'collectorNumber', label: 'Collector Number' },
  { value: 'mtgcbCollectorNumberNumeric', label: 'MTG CB Collector Number' },
  { value: 'rarityNumeric', label: 'Rarity' },
  { value: 'convertedManaCost', label: 'CMC' },
  { value: 'powerNumeric', label: 'Power' },
  { value: 'toughnessNumeric', label: 'Toughness' },
  { value: 'loyaltyNumeric', label: 'Loyalty' },
  { value: 'market', label: 'Price (Market)' },
  { value: 'low', label: 'Price (Low)' },
  { value: 'average', label: 'Price (Average)' },
  { value: 'high', label: 'Price (High)' },
  { value: 'foil', label: 'Price (Foil)' },
  { value: 'quantityAll', label: 'Quantity (All)' },
  { value: 'quantityNormal', label: 'Quantity (Normal)' },
  { value: 'quantityFoil', label: 'Quantity (Foil)' },
];

export const expansionSortByOptions = [
  { value: 'name', label: 'Name' },
  { value: 'releasedAt', label: 'Release Date' },
  { value: 'code', label: 'Set Code' },
  { value: 'cardCount', label: 'Cards In Set' },
];

export const rarityOptions = [
  { category: 'Rarities', value: 'common', label: 'Common', exclude: false },
  { category: 'Rarities', value: 'uncommon', label: 'Uncommon', exclude: false },
  { category: 'Rarities', value: 'rare', label: 'Rare', exclude: false },
  { category: 'Rarities', value: 'mythic', label: 'Mythic', exclude: false },
  { category: 'Rarities', value: 'special', label: 'Special', exclude: false },
  { category: 'Rarities', value: 'none', label: 'None', exclude: false },
];

export const expansionCategoryOptions = [
  { category: 'Set Categories', value: 'Normal', label: 'Normal', exclude: false },
  { category: 'Set Categories', value: 'Sealed', label: 'Sealed', exclude: false },
  { category: 'Set Categories', value: 'Special', label: 'Special', exclude: false },
];

interface SetState {
  searchQuery: string;
  oracleTextQuery: string;
  cardTypes: CardType[];
  cardRarities: CardRarity[];
  cardColors: CardColors;
  showAllPrintings: boolean;
  cardStatSearches: CardStatSearch[];
  sortBy: string;
  sortByDirection: 'asc' | 'desc';
  isFormVisible: boolean;
  viewSubject: 'cards' | 'sets';
  viewMode: 'grid' | 'table';
  priceType: PriceTypes;
}

export type PriceTypes = 'low' | 'average' | 'high' | 'market' | 'foil';

export interface CardType {
  category: string;
  label: string;
  value: string;
  exclude: boolean;
}

export interface CardSet {
  category: string;
  label: string;
  value: string;
  exclude: boolean;
}

export interface CardRarity {
  category: string;
  label: string;
  value: 'common' | 'uncommon' | 'rare' | 'mythic' | 'special' | 'none' | '';
  exclude: boolean;
}

export interface CardColors {
  white: boolean;
  blue: boolean;
  black: boolean;
  red: boolean;
  green: boolean;
  colorless: boolean;
  type: ColorTypes;
}

export interface CardStatSearch {
  searchAttribute: SearchAttribute;
  comparator: SearchComparators;
  value?: string;
}

export interface SearchAttributeOption {
  value: SearchAttribute;
  label: string;
}

export type SearchComparators = 'gt' | 'gte' | 'lt' | 'lte' | 'eq' | 'not';
export type SearchAttribute =
  | 'convertedManaCost'
  | 'powerNumeric'
  | 'toughnessNumeric'
  | 'loyaltyNumeric'
  | 'cardsAll'
  | 'cardsNormal'
  | 'cardsFoil';

export type ColorTypes = 'at-least-these-colors' | 'only-these-colors' | 'at-most-these-colors';

export interface CardTypes {
  cardTypes: CardType[];
}

export interface CardSets {
  cardSets: CardSet[];
}

export interface CardRarities {
  cardRarities: CardRarity[];
}

interface FormVisibility {
  isFormVisibile: boolean;
}

interface SearchQuery {
  searchQuery: string;
}

interface ExpansionSearchQuery {
  expansionSearchQuery: string;
}

interface OracleTextQuery {
  oracleTextQuery: string;
}

interface SearchAttributeChangePayload {
  searchAttribute: SearchAttribute;
  index: number;
}

interface SearchComparatorPayload {
  comparator: SearchComparators;
  index: number;
}

interface SearchValuePayload {
  value: string;
  index: number;
}

export interface SetType {
  category: string;
  label: string;
  value: string;
  exclude: boolean;
}

interface SetTypes {
  setTypes: SetType[];
}

export interface SetCategory {
  category: string;
  label: string;
  value: string;
  exclude: boolean;
}

export interface SetCategories {
  setCategories: SetCategory[];
}

export default setCollectionSlice.reducer;
