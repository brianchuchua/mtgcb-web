import { useSelector, useDispatch } from 'react-redux';
import styled from 'styled-components';
import FormControl from '@material-ui/core/FormControl';
import OutlinedInput from '@material-ui/core/OutlinedInput';
import InputAdornment from '@material-ui/core/InputAdornment';
import InputLabel from '@material-ui/core/InputLabel';
import SearchIcon from '@material-ui/icons/Search';
import { setOracleTextQuery } from '../../browseSlice';
import { RootState } from '../../../../redux/rootReducer';

const OracleTextSearch: React.FC = () => {
  const dispatch = useDispatch();
  const { oracleTextQuery } = useSelector((state: RootState) => state.browse);

  const updateOracleTextQuery = (newOracleTextQuery: string) => {
    dispatch(setOracleTextQuery({ oracleTextQuery: newOracleTextQuery }));
  };

  return (
    <div>
      <StyledOracleTextSearch fullWidth variant="outlined">
        <InputLabel htmlFor="oracle-text-query" className="input-label-fix">
          Oracle Text
        </InputLabel>
        <OutlinedInput
          id="oracle-text-query"
          value={oracleTextQuery}
          placeholder="Search by oracle text"
          label="Oracle Text"
          onChange={(e) => updateOracleTextQuery(e.target.value)}
          startAdornment={
            <InputAdornment position="start">
              <SearchIcon color="disabled" />
            </InputAdornment>
          }
        />
      </StyledOracleTextSearch>
    </div>
  );
};

const StyledOracleTextSearch = styled(FormControl)(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default OracleTextSearch;
