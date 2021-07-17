import { CardColors, CardType, CardSet, CardRarity, CardStatSearch } from '../../../features/browse/browseSlice';

export interface SearchOptions {
  first?: number;
  skip?: number;
  sortBy?: string[];
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
