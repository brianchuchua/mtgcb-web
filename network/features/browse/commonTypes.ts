import { CardColors, CardRarity, CardSet, CardStatSearch, CardType } from '../../../features/browse/browseSlice';

export interface SearchOptions {
  first?: number;
  skip?: number;
  sortBy?: string;
  sortByDirection?: 'asc' | 'desc';
  name?: string;
  oracleTextQuery?: string;
  artistQuery: string;
  cardTypes?: CardType[];
  cardSets?: CardSet[];
  cardRarities?: CardRarity[];
  cardColors: CardColors;
  showAllPrintings: boolean;
  cardStatSearches: CardStatSearch[];
  subsets?: string[];
}

export interface SetSearchOptions {
  first?: number;
  skip?: number;
  sortBy?: string[];
  sortByDirection?: 'asc' | 'desc';
  name?: string;
}

export interface TcgplayerMassImportOptions {
  setId: number;
  allCount?: number;
  mythicCount?: number;
  rareCount?: number;
  uncommonCount?: number;
  commonCount?: number;
  draftCubeCount?: number;
}

export interface TcgplayerMassImportForUserLegacyOptions {
  setId: number;
  userId: number;
  allCount?: number;
  mythicCount?: number;
  rareCount?: number;
  uncommonCount?: number;
  commonCount?: number;
  draftCubeCount?: number;
  includeSubsetsInSets: boolean;
}

export interface CardAutocompleteOptions {
  name?: string;
}
