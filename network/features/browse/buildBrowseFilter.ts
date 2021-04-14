import { CardType, CardColors } from '../../../features/browse/browseSlice';
import { addCardColorFilter, addCardTypeFilter } from './filters';

interface BuildBrowseFilterSettings {
  cardTypes?: CardType[];
  cardColors: CardColors;
}

interface BuildBrowseFilterFunction {
  (filterSettings: BuildBrowseFilterSettings): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const buildBrowseFilter: BuildBrowseFilterFunction = ({ cardTypes, cardColors }) => {
  const where = { AND: [] };

  addCardTypeFilter(cardTypes, where);
  addCardColorFilter(cardColors, where);

  return where;
};

export default buildBrowseFilter;
