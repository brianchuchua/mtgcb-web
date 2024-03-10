import { useDispatch, useSelector } from 'react-redux';
import CardStatSearch from '../../../../components/search-form-components/CardStatSearch';
import { RootState } from '../../../../redux/rootReducer';
import { convertCardStatSearchesToString } from '../../../../util/queryStringMappers';
import { useQueryParameter } from '../../../../util/useQueryParameter';
import { SearchAttribute, SearchComparators } from '../../../browse/browseSlice';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedCardStatSearchProps extends ConnectedSearchFormComponentProps {
  addCardStatSearch: any;
  removeCardStatSearch: any;
  setCardStatSearches: any;
  setComparator: any;
  setSearchAttribute: any;
  searchAttributeOptionsOverride?: any;
}

const ConnectedCardStatSearch: React.FC<ConnectedCardStatSearchProps> = ({
  reduxSlice,
  addCardStatSearch,
  removeCardStatSearch,
  setCardStatSearches,
  setComparator,
  setSearchAttribute,
  searchAttributeOptionsOverride,
}) => {
  const dispatch = useDispatch();
  const { cardStatSearches } = useSelector((state: RootState) => state[reduxSlice]);
  const setQueryParameter = useQueryParameter();

  const updateSearchAttribute = (searchAttribute: SearchAttribute, index: number) => {
    const payload = { searchAttribute, index };
    dispatch(setSearchAttribute(payload));

    const newCardStatSearches = [...cardStatSearches];
    newCardStatSearches[index] = { ...newCardStatSearches[index], searchAttribute };
    setQueryParameter('stats', convertCardStatSearchesToString(newCardStatSearches));
  };

  const updateComparator = (comparator: SearchComparators, index: number) => {
    const payload = { comparator, index };
    dispatch(setComparator(payload));

    const newCardStatSearches = [...cardStatSearches];
    newCardStatSearches[index] = { ...newCardStatSearches[index], comparator };
    setQueryParameter('stats', convertCardStatSearchesToString(newCardStatSearches));
  };

  const updateCardStatSearches = (value: string, index: number) => {
    const payload = { value, index };
    dispatch(setCardStatSearches(payload));

    const newCardStatSearches = [...cardStatSearches];
    newCardStatSearches[index] = { ...newCardStatSearches[index], value };
    setQueryParameter('stats', convertCardStatSearchesToString(newCardStatSearches));
  };

  const handleAddCardStatSearch = () => {
    dispatch(addCardStatSearch());

    const newCardStatSearches = [...cardStatSearches];
    newCardStatSearches.push({
      searchAttribute: 'convertedManaCost',
      comparator: 'gt',
      value: '',
    });
    setQueryParameter('stats', convertCardStatSearchesToString(newCardStatSearches));
  };

  const handleRemoveCardStatSearch = () => {
    dispatch(removeCardStatSearch());

    const newCardStatSearches = [...cardStatSearches];
    newCardStatSearches.pop();
    setQueryParameter('stats', convertCardStatSearchesToString(newCardStatSearches));
  };

  return (
    <CardStatSearch
      cardStatSearches={cardStatSearches}
      updateSearchAttribute={updateSearchAttribute}
      updateComparator={updateComparator}
      updateCardStatSearches={updateCardStatSearches}
      handleAddCardStatSearch={handleAddCardStatSearch}
      handleRemoveCardStatSearch={handleRemoveCardStatSearch}
      searchAttributeOptionsOverride={searchAttributeOptionsOverride}
    />
  );
};

export default ConnectedCardStatSearch;
