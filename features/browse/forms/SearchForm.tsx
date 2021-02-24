import { useSelector, useDispatch } from 'react-redux';
import styled from 'styled-components';
import FormControl from '@material-ui/core/FormControl';
import OutlinedInput from '@material-ui/core/OutlinedInput';
import InputAdornment from '@material-ui/core/InputAdornment';
import SearchIcon from '@material-ui/icons/Search';
import { RootState } from '../../../redux/rootReducer';
import { setSearchQuery } from '../browseSlice';

const SearchForm: React.FC = () => {
  const dispatch = useDispatch();
  const { isFormVisible, searchQuery } = useSelector((state: RootState) => state.browse);

  const updateSearchQuery = (newSearchQuery: string) => {
    dispatch(setSearchQuery({ searchQuery: newSearchQuery }));
  };

  return (
    <>
      {isFormVisible && (
        <>
          <SeachFormWrapper fullWidth variant="outlined">
            <OutlinedInput
              id="outlined-adornment-amount"
              value={searchQuery}
              placeholder="Search"
              onChange={(e) => updateSearchQuery(e.target.value)}
              startAdornment={
                <InputAdornment position="start">
                  <SearchIcon color="disabled" />
                </InputAdornment>
              }
              labelWidth={60}
              notched={false}
            />
          </SeachFormWrapper>
        </>
      )}
    </>
  );
};

const SeachFormWrapper = styled(FormControl)(() => ({
  padding: '8px',
}));

export default SearchForm;
