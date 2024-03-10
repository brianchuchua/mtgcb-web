import { useDispatch, useSelector } from 'react-redux';
import SortSelector from '../../../../components/search-form-components/SortSelector';
import { RootState } from '../../../../redux/rootReducer';
import { useQueryParameter } from '../../../../util/useQueryParameter';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedSortSelectorProps extends ConnectedSearchFormComponentProps {
  setCardSort: any;
  setCardSortDirection: any;
  sortByOptions?: any;
}

const ConnectedSortSelector: React.FC<ConnectedSortSelectorProps> = ({ reduxSlice, sortByOptions, setCardSort, setCardSortDirection }) => {
  const dispatch = useDispatch();
  const { sortBy, sortByDirection } = useSelector((state: RootState) => state[reduxSlice]);
  const setQueryParameter = useQueryParameter();

  const handleSortByChange = (event: React.ChangeEvent<{ value: string }>) => {
    dispatch(setCardSort(event.target.value));
    setQueryParameter('sort', event.target.value);
  };

  const handleSortByDirectionChange = (event: React.ChangeEvent<{ value: 'asc' | 'desc' }>) => {
    dispatch(setCardSortDirection(event.target.value));
    setQueryParameter('order', event.target.value);
  };

  return (
    <SortSelector
      sortBy={sortBy}
      sortByDirection={sortByDirection?.toLowerCase()}
      sortByOptions={sortByOptions}
      handleSortByChange={handleSortByChange}
      handleSortByDirectionChange={handleSortByDirectionChange}
    />
  );
};

export default ConnectedSortSelector;
