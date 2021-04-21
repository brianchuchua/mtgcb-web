import { CardColors, CardType } from '../../../features/browse/browseSlice';

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
  cardColors: CardColors;
  showAllPrintings: boolean;
}
