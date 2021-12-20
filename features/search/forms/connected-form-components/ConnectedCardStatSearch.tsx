import { useDispatch, useSelector } from 'react-redux';
import CardStatSearch from '../../../../components/search-form-components/CardStatSearch';
import { RootState } from '../../../../redux/rootReducer';
import { SearchAttribute, SearchComparators } from '../../../browse/browseSlice';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedCardStatSearchProps extends ConnectedSearchFormComponentProps {
  addCardStatSearch: any;
  removeCardStatSearch: any;
  setCardStatSearches: any;
  setComparator: any;
  setSearchAttribute: any;
}

const ConnectedCardStatSearch: React.FC<ConnectedCardStatSearchProps> = ({
  reduxSlice,
  addCardStatSearch,
  removeCardStatSearch,
  setCardStatSearches,
  setComparator,
  setSearchAttribute,
}) => {
  const dispatch = useDispatch();
  const { cardStatSearches } = useSelector((state: RootState) => state[reduxSlice]);

  const updateSearchAttribute = (searchAttribute: SearchAttribute, index: number) => {
    const payload = { searchAttribute, index };
    dispatch(setSearchAttribute(payload));
  };

  const updateComparator = (comparator: SearchComparators, index: number) => {
    const payload = { comparator, index };
    dispatch(setComparator(payload));
  };

  const updateCardStatSearches = (value: string, index: number) => {
    const payload = { value, index };
    dispatch(setCardStatSearches(payload));
  };

  const handleAddCardStatSearch = () => {
    dispatch(addCardStatSearch());
  };

  const handleRemoveCardStatSearch = () => {
    dispatch(removeCardStatSearch());
  };

  return (
    <CardStatSearch
      cardStatSearches={cardStatSearches}
      updateSearchAttribute={updateSearchAttribute}
      updateComparator={updateComparator}
      updateCardStatSearches={updateCardStatSearches}
      handleAddCardStatSearch={handleAddCardStatSearch}
      handleRemoveCardStatSearch={handleRemoveCardStatSearch}
    />
  );
};

export default ConnectedCardStatSearch;
