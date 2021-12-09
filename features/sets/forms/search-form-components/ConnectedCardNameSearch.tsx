import { useDispatch, useSelector } from 'react-redux';
import CardNameSearch from '../../../../components/search-form-components/CardNameSearch';
import { RootState } from '../../../../redux/rootReducer';
import { setSearchQuery } from '../../setSlice';

const ConnectedCardNameSearch: React.FC = () => {
  const dispatch = useDispatch();
  const { searchQuery } = useSelector((state: RootState) => state.set);

  const updateSearchQuery = (newSearchQuery: string) => {
    dispatch(setSearchQuery({ searchQuery: newSearchQuery }));
  };

  return <CardNameSearch searchQuery={searchQuery} updateSearchQuery={updateSearchQuery} />;
};

export default ConnectedCardNameSearch;
