import { CardColors, CardRarity, CardSet, CardStatSearch, CardType } from '../../../features/browse/browseSlice';

export interface SearchOptions {
  first?: number;
  skip?: number;
  sortBy?: string;
  sortByDirection?: 'ASC' | 'DESC';
  name?: string;
  oracleTextQuery?: string;
  cardTypes?: CardType[];
  cardSets?: CardSet[];
  cardRarities?: CardRarity[];
  cardColors: CardColors;
  showAllPrintings: boolean;
  cardStatSearches: CardStatSearch[];
}

export interface SetSearchOptions {
  first?: number;
  skip?: number;
  sortBy?: string[];
  sortByDirection?: 'ASC' | 'DESC';
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
