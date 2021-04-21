import { CardType, CardColors } from '../../../features/browse/browseSlice';
import { addCardColorFilter, addCardTypeFilter, addOracleTextFilter } from './filters';

interface BuildBrowseFilterSettings {
  cardTypes?: CardType[];
  cardColors: CardColors;
  oracleTextQuery?: string;
}

interface BuildBrowseFilterFunction {
  (filterSettings: BuildBrowseFilterSettings): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const buildBrowseFilter: BuildBrowseFilterFunction = ({ cardTypes, cardColors, oracleTextQuery }) => {
  const where = { AND: [] };

  addCardTypeFilter(cardTypes, where);
  addCardColorFilter(cardColors, where);
  addOracleTextFilter(oracleTextQuery, where);

  return where;
};

export default buildBrowseFilter;
