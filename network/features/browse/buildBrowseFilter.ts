import { CardType, CardColors } from '../../../features/browse/browseSlice';
import { addCardColorFilter, addCardTypeFilter } from './filters';

interface BuildBrowseFilterSettings {
  cardTypes?: CardType[];
  cardColors: CardColors;
}

const buildBrowseFilter = ({ cardTypes, cardColors }: BuildBrowseFilterSettings) => {
  const where = { AND: [] };

  addCardTypeFilter(cardTypes, where);
  addCardColorFilter(cardColors, where);

  return where;
};

export default buildBrowseFilter;
