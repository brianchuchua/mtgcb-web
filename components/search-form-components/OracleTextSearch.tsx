import FormControl from '@material-ui/core/FormControl';
import InputAdornment from '@material-ui/core/InputAdornment';
import InputLabel from '@material-ui/core/InputLabel';
import OutlinedInput from '@material-ui/core/OutlinedInput';
import Tooltip from '@material-ui/core/Tooltip';
import InfoOutlined from '@material-ui/icons/InfoOutlined';
import styled from 'styled-components';

interface OracleTextSearchProps {
  oracleTextQuery: string;
  updateOracleTextQuery: (oracleTextQuery: string) => void;
}

const OracleTextSearch: React.FC<OracleTextSearchProps> = ({ oracleTextQuery, updateOracleTextQuery }) => (
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
          <Tooltip
            placement="right"
            title={
              <>
                The following special symbols are supported:
                <ul>
                  <li>
                    {`{T} – tap symbol`} <i className="ms ms-tap" />
                  </li>
                  <li>
                    {`{Q} – untap symbol`} <i className="ms ms-untap" />
                  </li>
                  <li>
                    {`{W} – white mana`} <i className="ms ms-w ms-cost" />
                  </li>
                  <li>
                    {`{U} – blue mana`} <i className="ms ms-u ms-cost" />
                  </li>
                  <li>
                    {`{B} – black mana`} <i className="ms ms-b ms-cost" />
                  </li>
                  <li>
                    {`{R} – red mana`} <i className="ms ms-r ms-cost" />
                  </li>
                  <li>
                    {`{G} – green mana`} <i className="ms ms-g ms-cost" />
                  </li>
                  <li>
                    {`{C} – colorless mana`} <i className="ms ms-c ms-cost" />
                  </li>
                  <li>
                    {`{X} – X generic mana`} <i className="ms ms-x ms-cost" />
                  </li>
                  <li>
                    {`{0} – zero mana`} <i className="ms ms-0 ms-cost" />
                  </li>
                  <li>
                    {`{1} – one generic mana`} <i className="ms ms-1 ms-cost" />
                  </li>
                  <li>
                    {`{2} – two generic mana`} <i className="ms ms-2 ms-cost" />
                  </li>
                  <li>
                    {`{3} – three generic mana (and so on)`} <i className="ms ms-3 ms-cost" />
                  </li>
                  <li>
                    {`{W/U} – white or blue mana`} <i className="ms ms-wu ms-cost" />
                  </li>
                  <li>
                    {`{W/B} – white or black mana`} <i className="ms ms-wb ms-cost" />
                  </li>
                  <li>
                    {`{B/R} – black or red mana`} <i className="ms ms-br ms-cost" />
                  </li>
                  <li>
                    {`{B/G} – black or green mana`} <i className="ms ms-bg ms-cost" />
                  </li>
                  <li>
                    {`{U/B} – blue or black mana`} <i className="ms ms-ub ms-cost" />
                  </li>
                  <li>
                    {`{U/R} – blue or red mana`} <i className="ms ms-ur ms-cost" />
                  </li>
                  <li>
                    {`{R/G} – red or green mana`} <i className="ms ms-rg ms-cost" />
                  </li>
                  <li>
                    {`{R/W} – red or white mana`} <i className="ms ms-rw ms-cost" />
                  </li>
                  <li>
                    {`{G/W} – green or white mana`} <i className="ms ms-gw ms-cost" />
                  </li>
                  <li>
                    {`{G/U} – green or blue mana`} <i className="ms ms-gu ms-cost" />
                  </li>
                  <li>
                    {`{2/W} – two generic mana or white mana`} <i className="ms ms-2w ms-cost" />
                  </li>
                  <li>
                    {`{2/U} – two generic mana or blue mana`} <i className="ms ms-2u ms-cost" />
                  </li>
                  <li>
                    {`{2/B} – two generic mana or black mana`} <i className="ms ms-2b ms-cost" />
                  </li>
                  <li>
                    {`{2/R} – two generic mana or red mana`} <i className="ms ms-2r ms-cost" />
                  </li>
                  <li>
                    {`{2/G} – two generic mana or green mana`} <i className="ms ms-2g ms-cost" />
                  </li>
                  <li>
                    {`{W/P} – white mana or two life`} <i className="ms ms-p ms-w ms-cost" />
                  </li>
                  <li>
                    {`{U/P} – blue mana or two life`} <i className="ms ms-p ms-u ms-cost" />
                  </li>
                  <li>
                    {`{B/P} – black mana or two life`} <i className="ms ms-p ms-b ms-cost" />
                  </li>
                  <li>
                    {`{R/P} – red mana or two life`} <i className="ms ms-p ms-r ms-cost" />
                  </li>
                  <li>
                    {`{G/P} – green mana or two life`} <i className="ms ms-p ms-g ms-cost" />
                  </li>
                  <li>
                    {`{S} – snow mana`} <i className="ms ms-s ms-cost" />
                  </li>
                  <li>
                    {`{E} – energy symbol`} <i className="ms ms-e" />
                  </li>
                  <li>
                    {`{CHAOS} – chaos symbol`} <i className="ms ms-chaos" />
                  </li>
                </ul>
              </>
            }
          >
            <InputAdornment position="start">
              <InfoOutlined color="disabled" />
            </InputAdornment>
          </Tooltip>
        }
      />
    </StyledOracleTextSearch>
  </div>
);

const StyledOracleTextSearch = styled(FormControl)(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default OracleTextSearch;
