import { useSelector } from 'react-redux';
import { RootState } from '../../../redux/rootReducer';
import { CardNameSearch, TypeSelector } from './search-form-components';

const SearchForm: React.FC = () => {
  const { isFormVisible } = useSelector((state: RootState) => state.browse);

  return (
    <>
      {isFormVisible && (
        <>
          <CardNameSearch />
          <TypeSelector />
        </>
      )}
    </>
  );
};

export default SearchForm;
