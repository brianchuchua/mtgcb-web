import { useDispatch, useSelector } from 'react-redux';
import SortSelector from '../../../../components/search-form-components/SortSelector';
import { RootState } from '../../../../redux/rootReducer';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedSortSelectorProps extends ConnectedSearchFormComponentProps {
  setCardSort: any;
  setCardSortDirection: any;
  sortByOptions?: any;
}

const ConnectedSortSelector: React.FC<ConnectedSortSelectorProps> = ({ reduxSlice, sortByOptions, setCardSort, setCardSortDirection }) => {
  const dispatch = useDispatch();
  const { sortBy, sortByDirection } = useSelector((state: RootState) => state[reduxSlice]);

  const handleSortByChange = (event: React.ChangeEvent<{ value: string }>) => {
    dispatch(setCardSort(event.target.value));
  };

  const handleSortByDirectionChange = (event: React.ChangeEvent<{ value: 'ASC' | 'DESC' }>) => {
    dispatch(setCardSortDirection(event.target.value));
  };

  return (
    <SortSelector
      sortBy={sortBy}
      sortByDirection={sortByDirection}
      sortByOptions={sortByOptions}
      handleSortByChange={handleSortByChange}
      handleSortByDirectionChange={handleSortByDirectionChange}
    />
  );
};

export default ConnectedSortSelector;
