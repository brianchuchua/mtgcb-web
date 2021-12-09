import { useDispatch, useSelector } from 'react-redux';
import SortSelector from '../../../../components/search-form-components/SortSelector';
import { RootState } from '../../../../redux/rootReducer';
import { setCardSort, setCardSortDirection } from '../../browseSlice';

const ConnectedSortSelector: React.FC = () => {
  const dispatch = useDispatch();
  const { sortBy, sortByDirection } = useSelector((state: RootState) => state.browse);

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
      handleSortByChange={handleSortByChange}
      handleSortByDirectionChange={handleSortByDirectionChange}
    />
  );
};

export default ConnectedSortSelector;
