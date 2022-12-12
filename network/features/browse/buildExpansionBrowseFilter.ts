import { SetCategory, SetType } from '../../../features/browse/browseSlice';
import { addSetCategoryFilter, addSetTypeFilter } from './filters';

interface BuildBrowseExpansionFilterSettings {
  name?: string;
  setTypes?: SetType[];
  setCategories?: SetCategory[];
}

interface BuildBrowseExpansionFilterFunction {
  (filterSettings: BuildBrowseExpansionFilterSettings): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const buildBrowseExpansionFilter: BuildBrowseExpansionFilterFunction = ({ name, setTypes, setCategories }) => {
  const where = { AND: [] };

  if (name) {
    where.AND.push({
      name: {
        contains: name,
        mode: 'insensitive',
      },
    });
  }

  addSetTypeFilter(setTypes, where);
  addSetCategoryFilter(setCategories, where);

  return where;
};

export default buildBrowseExpansionFilter;
