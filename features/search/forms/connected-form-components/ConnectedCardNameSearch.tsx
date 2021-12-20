import { useDispatch, useSelector } from 'react-redux';
import CardNameSearch from '../../../../components/search-form-components/CardNameSearch';
import { RootState } from '../../../../redux/rootReducer';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedCardNameSearchProps extends ConnectedSearchFormComponentProps {
  setSearchQuery: any;
}

const ConnectedCardNameSearch: React.FC<ConnectedCardNameSearchProps> = ({ reduxSlice, setSearchQuery }) => {
  const dispatch = useDispatch();
  const { searchQuery } = useSelector((state: RootState) => state[reduxSlice]);

  const updateSearchQuery = (newSearchQuery: string) => {
    dispatch(setSearchQuery({ searchQuery: newSearchQuery }));
  };

  return <CardNameSearch searchQuery={searchQuery} updateSearchQuery={updateSearchQuery} />;
};

export default ConnectedCardNameSearch;
