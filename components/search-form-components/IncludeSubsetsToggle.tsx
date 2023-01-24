import FormControlLabel from '@material-ui/core/FormControlLabel';
import FormGroup from '@material-ui/core/FormGroup';
import Paper from '@material-ui/core/Paper';
import Switch from '@material-ui/core/Switch';
import Tooltip from '@material-ui/core/Tooltip';
import InfoOutlined from '@material-ui/icons/InfoOutlined';
import styled from 'styled-components';

interface IncludeSubsetsToggleProps {
  includeSubsets: boolean;
  updateIncludeSubsets: (showAllPrintings: boolean) => void;
}

const IncludeSubsetsToggle: React.FC<IncludeSubsetsToggleProps> = ({ includeSubsets, updateIncludeSubsets }) => {
  const handleIncludeSubsetsChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    updateIncludeSubsets(event.target.checked);
  };

  return (
    <ToggleGroupWrapper variant="outlined">
      <FormGroup>
        <StyledFormControlLabel
          control={<Switch checked={includeSubsets} onChange={handleIncludeSubsetsChange} name="includeSubsets" color="primary" />}
          label={
            <div style={{ display: 'flex', alignItems: 'center', flexWrap: 'wrap' }}>
              Show Subsets
              <Tooltip
                placement="right"
                title={
                  <>
                    A subset is a piece of a larger set.
                    <br /> <br />
                    Example: Tempest, a normal set, has a single subset, Tempest Prerelease Promos. <br /> <br />
                    You can view subsets by scrolling down a normal set's page -- or by enabling this flag to see it on the main set
                    gallery.
                    <br /> <br />
                    This setting is off by default.
                  </>
                }
              >
                <InfoOutlined color="disabled" style={{ marginLeft: '5px' }} />
              </Tooltip>
            </div>
          }
        />
      </FormGroup>
    </ToggleGroupWrapper>
  );
};

const ToggleGroupWrapper = styled(Paper)(() => ({ margin: '8px', marginTop: '0px' }));
const StyledFormControlLabel = styled(FormControlLabel)(() => ({ display: 'flex', justifyContent: 'center' }));

export default IncludeSubsetsToggle;
