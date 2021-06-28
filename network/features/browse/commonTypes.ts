import { CardColors, CardType, CardSet, CardStatSearch } from '../../../features/browse/browseSlice';

enum SortByOptions {
  nameAsc = 'name_ASC',
  nameDesc = 'name_DESC',
}

export interface SearchOptions {
  first?: number;
  skip?: number;
  sortBy?: SortByOptions;
  name?: string;
  oracleTextQuery?: string;
  cardTypes?: CardType[];
  cardSets?: CardSet[];
  cardColors: CardColors;
  showAllPrintings: boolean;
  cardStatSearches: CardStatSearch[];
}
