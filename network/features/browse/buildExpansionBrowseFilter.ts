import { SetCategory, SetType } from '../../../features/browse/browseSlice';
import { addSetCategoryFilter, addSetTypeFilter } from './filters';

interface BuildBrowseExpansionFilterSettings {
  setTypes?: SetType[];
  setCategories?: SetCategory[];
}

interface BuildBrowseExpansionFilterFunction {
  (filterSettings: BuildBrowseExpansionFilterSettings): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const buildBrowseExpansionFilter: BuildBrowseExpansionFilterFunction = ({ setTypes, setCategories }) => {
  const where = { AND: [] };

  addSetTypeFilter(setTypes, where);
  addSetCategoryFilter(setCategories, where);

  return where;
};

export default buildBrowseExpansionFilter;
