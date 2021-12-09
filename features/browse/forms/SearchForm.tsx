import { useSelector } from 'react-redux';
import { RootState } from '../../../redux/rootReducer';
import {
  CardNameSearch,
  CardStatSearch,
  ColorSelector,
  OracleTextSearch,
  PriceTypeSelector,
  RaritySelector,
  SetCategorySelector,
  SetNameSearch,
  SetSelector,
  SetSortSelector,
  SetTypeSelector,
  SortSelector,
  TypeSelector,
  ViewModeSelector,
} from './search-form-components';

// TODO: Add headers to this thing
const SearchForm: React.FC = () => {
  const { isFormVisible, viewSubject } = useSelector((state: RootState) => state.browse);

  return (
    <>
      {isFormVisible && viewSubject === 'cards' && (
        <>
          <ViewModeSelector />
          <PriceTypeSelector />
          <CardNameSearch />
          <OracleTextSearch />
          <TypeSelector />
          <ColorSelector />
          <RaritySelector />
          <SetSelector />
          <CardStatSearch />
          <SortSelector />
        </>
      )}
      {isFormVisible && viewSubject === 'sets' && (
        <>
          <ViewModeSelector />
          <PriceTypeSelector />
          <SetNameSearch />
          <SetCategorySelector />
          <SetTypeSelector />
          <SetSortSelector />
        </>
      )}
    </>
  );
};

export default SearchForm;
