import { CardColors, CardType, CardSet, CardStatSearch } from '../../../features/browse/browseSlice';

export interface SearchOptions {
  first?: number;
  skip?: number;
  sortBy?: string[];
  sortByDirection?: 'ASC' | 'DESC';
  name?: string;
  oracleTextQuery?: string;
  cardTypes?: CardType[];
  cardSets?: CardSet[];
  cardColors: CardColors;
  showAllPrintings: boolean;
  cardStatSearches: CardStatSearch[];
}
