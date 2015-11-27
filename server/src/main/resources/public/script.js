var Preview = React.createClass({
  colData: function(col, i) {
    return <td key={i}><img src={"data:image/png;base64," + col} className="img-responsive" alt="Template image" /></td>;
  },
  rowData: function(row, i) {
    return <tr key={i}>{row.map(this.colData)}</tr>;
  },
  render: function() {
    return (
      <div className="table-responsive">
        <table className="table">
          <tbody>
            {this.props.data.map(this.rowData)}
          </tbody>
        </table>
      </div>
    );
  }
});

var Form = React.createClass({
  postJSON: function() {
    $.ajax({
      url: this.props.url,
      dataType: 'json',
      type: 'POST',
      data: JSON.stringify(this.props.json(this.refs.canvas.toDataURL())),
      contentType: 'application/json',
      success: this.props.success
    });
  },
  loadImage: function(src) {
    var img = new Image();
    img.onload = function() {
      var width = img.width;
      var height = img.height;
      var size = Math.max(width, height);
      if (size > 1024) {
        var r = 1024 / size;
        width *= r;
        height *= r;
      }
      this.refs.canvas.width = width;
      this.refs.canvas.height = height;
      var ctx = this.refs.canvas.getContext("2d");
      ctx.drawImage(img, 0, 0, width, height);
      this.postJSON();
    }.bind(this);
    img.src = src;
  },
  componentDidMount: function() {
    this.loadImage(this.props.src);
  },
  handleChange: function(e) {
    var reader = new FileReader();
    reader.onload = function(e) {
      this.loadImage(e.target.result);
    }.bind(this);
    reader.readAsDataURL(e.target.files[0]);
  },
  render: function() {
    return (
      <form onChange={this.handleChange}>
        <label htmlFor="file">{this.props.label}</label>
        <input ref="file" type="file" name="file" capture="camera" />
        <p className="help-block">Image: <canvas ref="canvas" className="img-responsive" /></p>
      </form>
    );
  }
});

var Result = React.createClass({
  getInitialState: function() {
    return {
      han: 0,
      fu: 0,
      yaku: [],
      point: ""
    };
  },
  handleChange: function() {
    $.ajax({
      url: 'calculate',
      dataType: 'json',
      type: 'POST',
      data: JSON.stringify({
        data: this.props.data,
        dealer: this.refs.dealer.checked,
        selfdrawn: this.refs.selfdrawn.checked,
        seat: parseInt(this.refs.seat.value),
        round: parseInt(this.refs.round.value),
        dora: parseInt(this.refs.dora.value)
      }),
      contentType: 'application/json',
      success: function(data) {
        this.setState(data);
      }.bind(this)
    });
  },
  render: function() {
    return (
      <div>
        <form onChange={this.handleChange}>
          <div className="checkbox">
            <label>
              <input ref="dealer" type="checkbox" /> 親
            </label>
          </div>
          <div className="checkbox">
            <label>
              <input ref="selfdrawn" type="checkbox" /> 自摸
            </label>
          </div>
          <label htmlFor="seat">自風</label>
          <select ref="seat" className="form-control" name="seat">
            <option value="0">東</option>
            <option value="1">南</option>
            <option value="2">西</option>
            <option value="3">北</option>
          </select>
          <label htmlFor="round">場風</label>
          <select ref="round" className="form-control" name="round">
            <option value="0">東</option>
            <option value="1">南</option>
            <option value="2">西</option>
            <option value="3">北</option>
          </select>
          <label htmlFor="dora">ドラ</label>
          <input ref="dora" className="form-control" type="number" name="dora" defaultValue="0" />
        </form>
        <div>
          <p>{this.state.fu}符 {this.state.han}飜 {this.state.point}</p>
          <ul>{this.state.yaku.map(function (yaku, i) { return <li key={i}>{yaku}</li>; })}</ul>
        </div>
      </div>
    );
  }
});

var Hand = React.createClass({
  getInitialState: function() {
    return {closed: [], open: []};
  },
  createJSON: function(data) {
    return {
      data: data,
      training: this.props.data
    };
  },
  onSuccess: function(data) {
    this.setState(data);
    this.refs.result.handleChange();
  },
  trainingData: function(i) {
    return this.props.data.image[i];
  },
  previewData: function() {
    return [this.state.closed.map(this.trainingData)].concat(this.state.open.map(function(indices) {
      return indices.map(this.trainingData);
    }.bind(this)));
  },
  render: function() {
    return (
      <div>
        <Form success={this.onSuccess} url="recognize" label="手牌画像" src="hand.jpg" json={this.createJSON} />
        <Preview data={this.previewData()} />
        <Result ref="result" data={this.state} />
      </div>
    );
  }
});

var Tile = React.createClass({
  getInitialState: function() {
    var tile = localStorage.getItem('tile');
    return tile ? JSON.parse(tile) : {image: [], width: 0, height: 0};
  },
  createJSON: function(data) {
    return {data: data};
  },
  onSuccess: function(data) {
    this.setState(data);
    localStorage.setItem('tile', JSON.stringify(data));
  },
  previewData: function() {
    return [this.state.image.slice(0, 9), this.state.image.slice(9, 18), this.state.image.slice(18, 27), this.state.image.slice(27)];
  },
  render: function() {
    return (
      <div>
        <Form success={this.onSuccess} url="train" label="牌画像" src="tile.jpg" json={this.createJSON} />
        <Preview data={this.previewData()} />
        <Hand data={this.state} />
      </div>
    );
  }
});

var App = React.createClass({
  render: function() {
    return (
      <div className="container-fluid">
        <Tile />
      </div>
    );
  }
});

ReactDOM.render(
  <App />,
  document.getElementById('container')
);
