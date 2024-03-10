import { createSlice, PayloadAction } from '@reduxjs/toolkit';
// eslint-disable-next-line import/no-cycle
import {
  computedQueryFromUrl as queryFromUrl,
  convertStringToCardStatSearches,
  convertStringToCardTypes,
  convertStringToColors,
  convertStringToRarities,
} from '../../util/queryStringMappers';
import { getValueFromLocalStorage, setValueToLocalStorage } from '../../util/useLocalStorage';

const viewModeFromLocalStorage = getValueFromLocalStorage('viewMode', 'grid');
const priceTypeFromLocalStorage = getValueFromLocalStorage('priceType', 'market');

const initialState: SetState = {
  searchQuery: queryFromUrl.card || '',
  oracleTextQuery: queryFromUrl.oracle || '',
  artistQuery: queryFromUrl.artist || '',
  cardTypes: convertStringToCardTypes(queryFromUrl.types) || [],
  cardSets: [],
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
  subsets: queryFromUrl.subsets ? queryFromUrl.subsets?.split(',') : ['All'],
};

const emptyState = {
  searchQuery: '',
  oracleTextQuery: '',
  artistQuery: '',
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
  subsets: ['All'],
};

const setSlice = createSlice({
  name: 'set',
  initialState,
  reducers: {
    reset: (state) => ({ ...state, ...emptyState } as SetState),
    setSearchQuery(state, action: PayloadAction<SearchQuery>) {
      const { searchQuery } = action.payload;
      state.searchQuery = searchQuery;
    },
    setOracleTextQuery(state, action: PayloadAction<OracleTextQuery>) {
      const { oracleTextQuery } = action.payload;
      state.oracleTextQuery = oracleTextQuery;
    },
    setArtistQuery(state, action: PayloadAction<ArtistQuery>) {
      const { artistQuery } = action.payload;
      state.artistQuery = artistQuery;
    },
    setCardTypes(state, action: PayloadAction<CardTypes>) {
      const { cardTypes } = action.payload;
      state.cardTypes = cardTypes;
    },
    setCardRarities(state, action: PayloadAction<CardRarities>) {
      const { cardRarities } = action.payload;
      state.cardRarities = cardRarities;
    },
    setCardColors(state, action: PayloadAction<string>) {
      const color = action.payload;
      state.cardColors[color] = !state.cardColors[color];
    },
    setColorType(state, action: PayloadAction<ColorTypes>) {
      const type = action.payload;
      state.cardColors.type = type;
    },
    setShowAllPrintings(state, action: PayloadAction<boolean>) {
      const showAllPrintings = action.payload;
      state.showAllPrintings = showAllPrintings;
    },
    setSearchAttribute(state, action: PayloadAction<SearchAttributeChangePayload>) {
      const { searchAttribute, index } = action.payload;
      state.cardStatSearches[index].searchAttribute = searchAttribute;
    },
    setComparator(state, action: PayloadAction<SearchComparatorPayload>) {
      const { comparator, index } = action.payload;
      state.cardStatSearches[index].comparator = comparator;
    },
    setCardStatSearches(state, action: PayloadAction<SearchValuePayload>) {
      const { value, index } = action.payload;
      state.cardStatSearches[index].value = value;
    },
    addCardStatSearch(state) {
      state.cardStatSearches.push({ searchAttribute: 'convertedManaCost', comparator: 'gt', value: '' });
    },
    removeCardStatSearch(state) {
      if (state.cardStatSearches.length > 1) {
        state.cardStatSearches.pop();
      }
    },
    setCardSort(state, action: PayloadAction<string>) {
      const sortBy = action.payload;
      state.sortBy = sortBy;
    },
    setCardSortDirection(state, action: PayloadAction<'asc' | 'desc'>) {
      const sortByDirection = action.payload;
      state.sortByDirection = sortByDirection;
    },
    setFormVisibility(state, action: PayloadAction<FormVisibility>) {
      const { isFormVisibile } = action.payload;
      state.isFormVisible = isFormVisibile;
    },
    setViewSubject(state, action: PayloadAction<'cards' | 'sets'>) {
      const viewSubject = action.payload;
      state.viewSubject = viewSubject;
      setValueToLocalStorage('viewSubject', viewSubject);
    },
    setViewMode(state, action: PayloadAction<'grid' | 'table'>) {
      const viewMode = action.payload;
      state.viewMode = viewMode;
      setValueToLocalStorage('viewMode', viewMode);
    },
    setPriceType(state, action: PayloadAction<PriceTypes>) {
      const priceType = action.payload;
      state.priceType = priceType;
      setValueToLocalStorage('priceType', priceType);
    },
    setSubsets(state, action: PayloadAction<string[]>) {
      const subsets = action.payload;
      state.subsets = subsets;
    },
    setCardSets(state, action: PayloadAction<CardSet[]>) {
      const cardSets = action.payload;
      state.cardSets = cardSets;
    },
  },
});

export const {
  setSearchQuery,
  setOracleTextQuery,
  setArtistQuery,
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
  setSubsets,
  setCardSets,
} = setSlice.actions;

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
  artistQuery: string;
  cardTypes: CardType[];
  cardSets: CardSet[];
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
  subsets: string[];
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

interface ArtistQuery {
  artistQuery: string;
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

export default setSlice.reducer;
