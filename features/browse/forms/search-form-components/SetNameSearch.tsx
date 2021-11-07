import FormControl from '@material-ui/core/FormControl';
import InputAdornment from '@material-ui/core/InputAdornment';
import InputLabel from '@material-ui/core/InputLabel';
import OutlinedInput from '@material-ui/core/OutlinedInput';
import SearchIcon from '@material-ui/icons/Search';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import { RootState } from '../../../../redux/rootReducer';
import { setExpansionSearchQuery } from '../../browseSlice';

const SetNameSearch: React.FC = () => {
  const dispatch = useDispatch();
  const { expansionSearchQuery } = useSelector((state: RootState) => state.browse);

  const updateSearchQuery = (newSearchQuery: string) => {
    dispatch(setExpansionSearchQuery({ expansionSearchQuery: newSearchQuery }));
  };

  return (
    <StyledSetNameSearch fullWidth variant="outlined">
      <InputLabel htmlFor="expansion-search-query" className="input-label-fix">
        Set Name
      </InputLabel>
      <OutlinedInput
        id="expansion-search-query"
        value={expansionSearchQuery}
        placeholder="Search by set name"
        label="Set Name"
        onChange={(e) => {
          updateSearchQuery(e.target.value);
        }}
        startAdornment={
          <InputAdornment position="start">
            <SearchIcon color="disabled" />
          </InputAdornment>
        }
      />
    </StyledSetNameSearch>
  );
};

const StyledSetNameSearch = styled(FormControl)(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default SetNameSearch;
